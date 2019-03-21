//===----------------------------------------------------------------------===//
//
// This source file is from of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// Modified by TellowKrinkle to complain if you ignore any fields
//
//===----------------------------------------------------------------------===//

import Foundation

//===----------------------------------------------------------------------===//
// MARK: - Private Foundation functions that we still need
//===----------------------------------------------------------------------===//

/// Returns a description of the type of `value` appropriate for an error message.
///
/// - parameter value: The value whose type to describe.
/// - returns: A string describing `value`.
/// - precondition: `value` is one of the types below.
fileprivate func _typeDescription(of value: Any) -> String {
	if value is NSNull {
		return "a null value"
	} else if value is NSNumber /* FIXME: If swift-corelibs-foundation isn't updated to use NSNumber, this check will be necessary: || value is Int || value is Double */ {
		return "a number"
	} else if value is String {
		return "a string/data"
	} else if value is [Any] {
		return "an array"
	} else if value is [String : Any] {
		return "a dictionary"
	} else {
		return "\(type(of: value))"
	}
}

/// Returns a `.typeMismatch` error describing the expected type.
///
/// - parameter path: The path of `CodingKey`s taken to decode a value of this type.
/// - parameter expectation: The type expected to be encountered.
/// - parameter reality: The value that was encountered instead of the expected type.
/// - returns: A `DecodingError` with the appropriate path and debug description.
fileprivate func makeTypeMismatchError(at path: [CodingKey], expectation: Any.Type, reality: Any) -> DecodingError {
	let description = "Expected to decode \(expectation) but found \(_typeDescription(of: reality)) instead."
	return .typeMismatch(expectation, DecodingError.Context(codingPath: path, debugDescription: description))
}

// Previously a part of `JSONEncoder.KeyEncodingStrategy`, but the rest of that is no longer needed
fileprivate func _convertToSnakeCase(_ stringKey: String) -> String {
	guard !stringKey.isEmpty else { return stringKey }

	var words : [Range<String.Index>] = []
	// The general idea of this algorithm is to split words on transition from lower to upper case, then on transition of >1 upper case characters to lowercase
	//
	// myProperty -> my_property
	// myURLProperty -> my_url_property
	//
	// We assume, per Swift naming conventions, that the first character of the key is lowercase.
	var wordStart = stringKey.startIndex
	var searchRange = stringKey.index(after: wordStart)..<stringKey.endIndex

	// Find next uppercase character
	while let upperCaseRange = stringKey.rangeOfCharacter(from: CharacterSet.uppercaseLetters, options: [], range: searchRange) {
		let untilUpperCase = wordStart..<upperCaseRange.lowerBound
		words.append(untilUpperCase)

		// Find next lowercase character
		searchRange = upperCaseRange.lowerBound..<searchRange.upperBound
		guard let lowerCaseRange = stringKey.rangeOfCharacter(from: CharacterSet.lowercaseLetters, options: [], range: searchRange) else {
			// There are no more lower case letters. Just end here.
			wordStart = searchRange.lowerBound
			break
		}

		// Is the next lowercase letter more than 1 after the uppercase? If so, we encountered a group of uppercase letters that we should treat as its own word
		let nextCharacterAfterCapital = stringKey.index(after: upperCaseRange.lowerBound)
		if lowerCaseRange.lowerBound == nextCharacterAfterCapital {
			// The next character after capital is a lower case character and therefore not a word boundary.
			// Continue searching for the next upper case for the boundary.
			wordStart = upperCaseRange.lowerBound
		} else {
			// There was a range of >1 capital letters. Turn those into a word, stopping at the capital before the lower case character.
			let beforeLowerIndex = stringKey.index(before: lowerCaseRange.lowerBound)
			words.append(upperCaseRange.lowerBound..<beforeLowerIndex)

			// Next word starts at the capital before the lowercase we just found
			wordStart = beforeLowerIndex
		}
		searchRange = lowerCaseRange.upperBound..<searchRange.upperBound
	}
	words.append(wordStart..<searchRange.upperBound)
	let result = words.map({ (range) in
		return stringKey[range].lowercased()
	}).joined(separator: "_")
	return result
}

fileprivate let kCFBooleanTrue = true as NSNumber
fileprivate let kCFBooleanFalse = false as NSNumber

//===----------------------------------------------------------------------===//
// MARK: - JSON Decoder
//===----------------------------------------------------------------------===//

/// `PedanticJSONDecoder` facilitates the decoding of JSON into semantic `Decodable` types.
open class PedanticJSONDecoder {
	fileprivate class IgnoredKeysTracker {
		struct DictionaryTracker {
			let tracker: IgnoredKeysTracker
			let list: Int
			init(path: [CodingKey], keys: Set<String>, on tracker: IgnoredKeysTracker) {
				self.tracker = tracker
				self.list = tracker.dictionaries.count
				tracker.dictionaries.append((path, keys))
			}
			func use(_ key: String) { tracker.dictionaries[list].list.remove(key) }
		}

		struct ArrayTracker {
			let tracker: IgnoredKeysTracker
			let list: Int
			init(path: [CodingKey], keys: Set<Int>, on tracker: IgnoredKeysTracker) {
				self.tracker = tracker
				self.list = tracker.arrays.count
				tracker.arrays.append((path, keys))
			}
			func use(_ key: Int) { tracker.arrays[list].list.remove(key) }
		}

		var dictionaries: [(path: [CodingKey], list: Set<String>)] = []
		var arrays: [(path: [CodingKey], list: Set<Int>)] = []

		func assertEmpty() throws {
			let nonemptyDictionaries = dictionaries.filter { !$0.list.isEmpty }
			let nonemptyArrays = arrays.filter { !$0.list.isEmpty }
			if !(nonemptyDictionaries.isEmpty && nonemptyArrays.isEmpty) {
				let dic = nonemptyDictionaries.map { IgnoredKeys(path: $0.path, ignoredKeys: $0.list.map { .dictionary($0) }) }
				let arr = nonemptyArrays.map { IgnoredKeys(path: $0.path, ignoredKeys: $0.list.map { .array($0) }) }
				throw IgnoredKeysError(keysets: dic + arr)
			}
		}
	}

	public enum JSONKey: CustomStringConvertible {
		case dictionary(String)
		case array(Int)

		public var description: String {
			switch self {
			case .dictionary(let key): return key
			case      .array(let key): return String(key)
			}
		}
	}

	public struct IgnoredKeys: CustomStringConvertible {

		public let path: [CodingKey]
		public let ignoredKeys: [JSONKey]

		public var description: String {
			return "Ignored the keys \(ignoredKeys) at path \(path.map { $0.stringValue })"
		}
	}

	public struct IgnoredKeysError: Error, CustomStringConvertible {
		public internal(set) var keysets: [IgnoredKeys]

		public var description: String {
			return keysets.lazy.map(String.init(describing:)).joined(separator: "\n")
		}
	}

	// MARK: Options

	/// The strategy to use for decoding `Date` values.
	public enum DateDecodingStrategy {
		/// Defer to `Date` for decoding. This is the default strategy.
		case deferredToDate

		/// Decode the `Date` as a UNIX timestamp from a JSON number.
		case secondsSince1970

		/// Decode the `Date` as UNIX millisecond timestamp from a JSON number.
		case millisecondsSince1970

		/// Decode the `Date` as an ISO-8601-formatted string (in RFC 3339 format).
		@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
		case iso8601

		/// Decode the `Date` as a string parsed by the given formatter.
		case formatted(DateFormatter)

		/// Decode the `Date` as a custom value decoded by the given closure.
		case custom((_ decoder: Decoder) throws -> Date)
	}

	/// The strategy to use for decoding `Data` values.
	public enum DataDecodingStrategy {
		/// Defer to `Data` for decoding.
		case deferredToData

		/// Decode the `Data` from a Base64-encoded string. This is the default strategy.
		case base64

		/// Decode the `Data` as a custom value decoded by the given closure.
		case custom((_ decoder: Decoder) throws -> Data)
	}

	/// The strategy to use for non-JSON-conforming floating-point values (IEEE 754 infinity and NaN).
	public enum NonConformingFloatDecodingStrategy {
		/// Throw upon encountering non-conforming values. This is the default strategy.
		case `throw`

		/// Decode the values from the given representation strings.
		case convertFromString(positiveInfinity: String, negativeInfinity: String, nan: String)
	}

	/// The strategy to use for automatically changing the value of keys before decoding.
	public enum KeyDecodingStrategy {
		/// Use the keys specified by each type. This is the default strategy.
		case useDefaultKeys

		/// Convert from "snake_case_keys" to "camelCaseKeys" before attempting to match a key with the one specified by each type.
		///
		/// The conversion to upper case uses `Locale.system`, also known as the ICU "root" locale. This means the result is consistent regardless of the current user's locale and language preferences.
		///
		/// Converting from snake case to camel case:
		/// 1. Capitalizes the word starting after each `_`
		/// 2. Removes all `_`
		/// 3. Preserves starting and ending `_` (as these are often used to indicate private variables or other metadata).
		/// For example, `one_two_three` becomes `oneTwoThree`. `_one_two_three_` becomes `_oneTwoThree_`.
		///
		/// - Note: Using a key decoding strategy has a nominal performance cost, as each string key has to be inspected for the `_` character.
		case convertFromSnakeCase

		/// Provide a custom conversion from the key in the encoded JSON to the keys specified by the decoded types.
		/// The full path to the current decoding position is provided for context (in case you need to locate this key within the payload). The returned key is used in place of the last component in the coding path before decoding.
		/// If the result of the conversion is a duplicate key, then only one value will be present in the container for the type to decode from.
		case custom((_ codingPath: [CodingKey]) -> CodingKey)

		fileprivate static func _convertFromSnakeCase(_ stringKey: String) -> String {
			guard !stringKey.isEmpty else { return stringKey }

			// Find the first non-underscore character
			guard let firstNonUnderscore = stringKey.firstIndex(where: { $0 != "_" }) else {
				// Reached the end without finding an _
				return stringKey
			}

			// Find the last non-underscore character
			var lastNonUnderscore = stringKey.index(before: stringKey.endIndex)
			while lastNonUnderscore > firstNonUnderscore && stringKey[lastNonUnderscore] == "_" {
				stringKey.formIndex(before: &lastNonUnderscore)
			}

			let keyRange = firstNonUnderscore...lastNonUnderscore
			let leadingUnderscoreRange = stringKey.startIndex..<firstNonUnderscore
			let trailingUnderscoreRange = stringKey.index(after: lastNonUnderscore)..<stringKey.endIndex

			var components = stringKey[keyRange].split(separator: "_")
			let joinedString : String
			if components.count == 1 {
				// No underscores in key, leave the word as is - maybe already camel cased
				joinedString = String(stringKey[keyRange])
			} else {
				joinedString = ([components[0].lowercased()] + components[1...].map { $0.capitalized }).joined()
			}

			// Do a cheap isEmpty check before creating and appending potentially empty strings
			let result : String
			if (leadingUnderscoreRange.isEmpty && trailingUnderscoreRange.isEmpty) {
				result = joinedString
			} else if (!leadingUnderscoreRange.isEmpty && !trailingUnderscoreRange.isEmpty) {
				// Both leading and trailing underscores
				result = String(stringKey[leadingUnderscoreRange]) + joinedString + String(stringKey[trailingUnderscoreRange])
			} else if (!leadingUnderscoreRange.isEmpty) {
				// Just leading
				result = String(stringKey[leadingUnderscoreRange]) + joinedString
			} else {
				// Just trailing
				result = joinedString + String(stringKey[trailingUnderscoreRange])
			}
			return result
		}
	}

	/// The strategy to use in decoding dates. Defaults to `.deferredToDate`.
	open var dateDecodingStrategy: DateDecodingStrategy = .deferredToDate

	/// The strategy to use in decoding binary data. Defaults to `.base64`.
	open var dataDecodingStrategy: DataDecodingStrategy = .base64

	/// The strategy to use in decoding non-conforming numbers. Defaults to `.throw`.
	open var nonConformingFloatDecodingStrategy: NonConformingFloatDecodingStrategy = .throw

	/// The strategy to use for decoding keys. Defaults to `.useDefaultKeys`.
	open var keyDecodingStrategy: KeyDecodingStrategy = .useDefaultKeys

	/// Contextual user-provided information for use during decoding.
	open var userInfo: [CodingUserInfoKey : Any] = [:]

	/// Options set on the top-level encoder to pass down the decoding hierarchy.
	fileprivate struct _Options {
		let dateDecodingStrategy: DateDecodingStrategy
		let dataDecodingStrategy: DataDecodingStrategy
		let nonConformingFloatDecodingStrategy: NonConformingFloatDecodingStrategy
		let keyDecodingStrategy: KeyDecodingStrategy
		let userInfo: [CodingUserInfoKey : Any]
	}

	/// The options set on the top-level decoder.
	fileprivate var options: _Options {
		return _Options(dateDecodingStrategy: dateDecodingStrategy,
		                dataDecodingStrategy: dataDecodingStrategy,
		                nonConformingFloatDecodingStrategy: nonConformingFloatDecodingStrategy,
		                keyDecodingStrategy: keyDecodingStrategy,
		                userInfo: userInfo)
	}

	// MARK: - Constructing a JSON Decoder

	/// Initializes `self` with default strategies.
	public init() {}

	// MARK: - Decoding Values

	/// Decodes a top-level value of the given type from the given JSON representation.
	///
	/// - parameter type: The type of the value to decode.
	/// - parameter data: The data to decode from.
	/// - returns: A value of the requested type.
	/// - throws: `DecodingError.dataCorrupted` if values requested from the payload are corrupted, or if the given data is not valid JSON.
	/// - throws: An error if any value throws an error during decoding.
	open func decode<T : Decodable>(_ type: T.Type, from data: Data) throws -> T {
		let topLevel: Any
		do {
			topLevel = try JSONSerialization.jsonObject(with: data)
		} catch {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: [], debugDescription: "The given data was not valid JSON.", underlyingError: error))
		}
		let tracker = IgnoredKeysTracker()

		let decoder = _PedanticJSONDecoder(referencing: topLevel, options: self.options, tracker: tracker)
		guard let value = try decoder.unbox(topLevel, as: type) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: [], debugDescription: "The given data did not contain a top-level value."))
		}

		try tracker.assertEmpty()

		return value
	}
}

// MARK: - _PedanticJSONDecoder

fileprivate class _PedanticJSONDecoder : Decoder {
	// MARK: Properties

	/// The decoder's storage.
	fileprivate var storage: _JSONDecodingStorage

	/// Options set on the top-level decoder.
	fileprivate let options: PedanticJSONDecoder._Options

	/// The path to the current point in encoding.
	fileprivate(set) public var codingPath: [CodingKey]

	fileprivate var tracker: PedanticJSONDecoder.IgnoredKeysTracker

	/// Contextual user-provided information for use during encoding.
	public var userInfo: [CodingUserInfoKey : Any] {
		return self.options.userInfo
	}

	// MARK: - Initialization

	/// Initializes `self` with the given top-level container and options.
	fileprivate init(referencing container: Any, at codingPath: [CodingKey] = [], options: PedanticJSONDecoder._Options, tracker: PedanticJSONDecoder.IgnoredKeysTracker) {
		self.storage = _JSONDecodingStorage()
		self.storage.push(container: container)
		self.codingPath = codingPath
		self.options = options
		self.tracker = tracker
	}

	// MARK: - Decoder Methods

	public func container<Key>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key> {
		guard !(self.storage.topContainer is NSNull) else {
			throw DecodingError.valueNotFound(KeyedDecodingContainer<Key>.self,
			                                  DecodingError.Context(codingPath: self.codingPath,
			                                                        debugDescription: "Cannot get keyed decoding container -- found null value instead."))
		}

		guard let topContainer = self.storage.topContainer as? [String : Any] else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: [String : Any].self, reality: self.storage.topContainer)
		}

		let container = _JSONKeyedDecodingContainer<Key>(referencing: self, wrapping: topContainer)
		return KeyedDecodingContainer(container)
	}

	public func unkeyedContainer() throws -> UnkeyedDecodingContainer {
		guard !(self.storage.topContainer is NSNull) else {
			throw DecodingError.valueNotFound(UnkeyedDecodingContainer.self,
			                                  DecodingError.Context(codingPath: self.codingPath,
			                                                        debugDescription: "Cannot get unkeyed decoding container -- found null value instead."))
		}

		guard let topContainer = self.storage.topContainer as? [Any] else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: [Any].self, reality: self.storage.topContainer)
		}

		return _JSONUnkeyedDecodingContainer(referencing: self, wrapping: topContainer)
	}

	public func singleValueContainer() throws -> SingleValueDecodingContainer {
		return self
	}
}

// MARK: - Decoding Storage

fileprivate struct _JSONDecodingStorage {
	// MARK: Properties

	/// The container stack.
	/// Elements may be any one of the JSON types (NSNull, NSNumber, String, Array, [String : Any]).
	private(set) fileprivate var containers: [Any] = []

	// MARK: - Initialization

	/// Initializes `self` with no containers.
	fileprivate init() {}

	// MARK: - Modifying the Stack

	fileprivate var count: Int {
		return self.containers.count
	}

	fileprivate var topContainer: Any {
		precondition(!self.containers.isEmpty, "Empty container stack.")
		return self.containers.last!
	}

	fileprivate mutating func push(container: Any) {
		self.containers.append(container)
	}

	fileprivate mutating func popContainer() {
		precondition(!self.containers.isEmpty, "Empty container stack.")
		self.containers.removeLast()
	}
}

// MARK: Decoding Containers

fileprivate struct _JSONKeyedDecodingContainer<K : CodingKey> : KeyedDecodingContainerProtocol {
	typealias Key = K

	// MARK: Properties

	/// A reference to the decoder we're reading from.
	private let decoder: _PedanticJSONDecoder

	/// A reference to the container we're reading from.
	private let container: [String : Any]

	/// For tracking the keys we've used
	private let tracker: PedanticJSONDecoder.IgnoredKeysTracker.DictionaryTracker

	/// The path of coding keys taken to get to this point in decoding.
	private(set) public var codingPath: [CodingKey]

	// MARK: - Initialization

	/// Initializes `self` by referencing the given decoder and container.
	fileprivate init(referencing decoder: _PedanticJSONDecoder, wrapping container: [String : Any]) {
		self.decoder = decoder
		switch decoder.options.keyDecodingStrategy {
		case .useDefaultKeys:
			self.container = container
		case .convertFromSnakeCase:
			// Convert the snake case keys in the container to camel case.
			// If we hit a duplicate key after conversion, then we'll use the first one we saw. Effectively an undefined behavior with JSON dictionaries.
			self.container = Dictionary(container.map {
				key, value in (PedanticJSONDecoder.KeyDecodingStrategy._convertFromSnakeCase(key), value)
			}, uniquingKeysWith: { (first, _) in first })
		case .custom(let converter):
			self.container = Dictionary(container.map {
				key, value in (converter(decoder.codingPath + [_JSONKey(stringValue: key, intValue: nil)]).stringValue, value)
			}, uniquingKeysWith: { (first, _) in first })
		}
		self.codingPath = decoder.codingPath
		self.tracker = .init(path: codingPath, keys: Set(container.keys), on: decoder.tracker)
	}

	// MARK: - KeyedDecodingContainerProtocol Methods

	public var allKeys: [Key] {
		return self.container.keys.compactMap { Key(stringValue: $0) }
	}

	public func contains(_ key: Key) -> Bool {
		tracker.use(key.stringValue)
		return self.container[key.stringValue] != nil
	}

	private func _errorDescription(of key: CodingKey) -> String {
		switch decoder.options.keyDecodingStrategy {
		case .convertFromSnakeCase:
			// In this case we can attempt to recover the original value by reversing the transform
			let original = key.stringValue
			let converted = _convertToSnakeCase(original)
			if converted == original {
				return "\(key) (\"\(original)\")"
			} else {
				return "\(key) (\"\(original)\"), converted to \(converted)"
			}
		default:
			// Otherwise, just report the converted string
			return "\(key) (\"\(key.stringValue)\")"
		}
	}

	public func decodeNil(forKey key: Key) throws -> Bool {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		return entry is NSNull
	}

	public func decode(_ type: Bool.Type, forKey key: Key) throws -> Bool {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: Bool.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: Int.Type, forKey key: Key) throws -> Int {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: Int.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: Int8.Type, forKey key: Key) throws -> Int8 {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: Int8.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: Int16.Type, forKey key: Key) throws -> Int16 {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: Int16.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: Int32.Type, forKey key: Key) throws -> Int32 {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: Int32.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: Int64.Type, forKey key: Key) throws -> Int64 {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: Int64.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: UInt.Type, forKey key: Key) throws -> UInt {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: UInt.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: UInt8.Type, forKey key: Key) throws -> UInt8 {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: UInt8.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: UInt16.Type, forKey key: Key) throws -> UInt16 {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: UInt16.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: UInt32.Type, forKey key: Key) throws -> UInt32 {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: UInt32.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: UInt64.Type, forKey key: Key) throws -> UInt64 {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: UInt64.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: Float.Type, forKey key: Key) throws -> Float {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: Float.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: Double.Type, forKey key: Key) throws -> Double {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: Double.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode(_ type: String.Type, forKey key: Key) throws -> String {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: String.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func decode<T : Decodable>(_ type: T.Type, forKey key: Key) throws -> T {
		tracker.use(key.stringValue)
		guard let entry = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "No value associated with key \(_errorDescription(of: key))."))
		}

		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = try self.decoder.unbox(entry, as: type) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath, debugDescription: "Expected \(type) value but found null instead."))
		}

		return value
	}

	public func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type, forKey key: Key) throws -> KeyedDecodingContainer<NestedKey> {
		tracker.use(key.stringValue)
		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key,
			                                DecodingError.Context(codingPath: self.codingPath,
			                                                      debugDescription: "Cannot get \(KeyedDecodingContainer<NestedKey>.self) -- no value found for key \(_errorDescription(of: key))"))
		}

		guard let dictionary = value as? [String : Any] else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: [String : Any].self, reality: value)
		}

		let container = _JSONKeyedDecodingContainer<NestedKey>(referencing: self.decoder, wrapping: dictionary)
		return KeyedDecodingContainer(container)
	}

	public func nestedUnkeyedContainer(forKey key: Key) throws -> UnkeyedDecodingContainer {
		tracker.use(key.stringValue)
		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		guard let value = self.container[key.stringValue] else {
			throw DecodingError.keyNotFound(key,
			                                DecodingError.Context(codingPath: self.codingPath,
			                                                      debugDescription: "Cannot get UnkeyedDecodingContainer -- no value found for key \(_errorDescription(of: key))"))
		}

		guard let array = value as? [Any] else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: [Any].self, reality: value)
		}

		return _JSONUnkeyedDecodingContainer(referencing: self.decoder, wrapping: array)
	}

	private func _superDecoder(forKey key: CodingKey) throws -> Decoder {
		self.decoder.codingPath.append(key)
		defer { self.decoder.codingPath.removeLast() }

		let value: Any = self.container[key.stringValue] ?? NSNull()
		return _PedanticJSONDecoder(referencing: value, at: self.decoder.codingPath, options: self.decoder.options, tracker: decoder.tracker)
	}

	public func superDecoder() throws -> Decoder {
		return try _superDecoder(forKey: _JSONKey.super)
	}

	public func superDecoder(forKey key: Key) throws -> Decoder {
		return try _superDecoder(forKey: key)
	}
}

fileprivate struct _JSONUnkeyedDecodingContainer : UnkeyedDecodingContainer {
	// MARK: Properties

	/// A reference to the decoder we're reading from.
	private let decoder: _PedanticJSONDecoder

	/// A reference to the container we're reading from.
	private let container: [Any]

	/// The path of coding keys taken to get to this point in decoding.
	private(set) public var codingPath: [CodingKey]

	/// The index of the element we're about to decode.
	private(set) public var currentIndex: Int

	/// For tracking the keys we've used
	private let tracker: PedanticJSONDecoder.IgnoredKeysTracker.ArrayTracker

	// MARK: - Initialization

	/// Initializes `self` by referencing the given decoder and container.
	fileprivate init(referencing decoder: _PedanticJSONDecoder, wrapping container: [Any]) {
		self.decoder = decoder
		self.container = container
		self.codingPath = decoder.codingPath
		self.currentIndex = 0
		self.tracker = .init(path: codingPath, keys: Set(container.indices), on: decoder.tracker)
	}

	// MARK: - UnkeyedDecodingContainer Methods

	public var count: Int? {
		return self.container.count
	}

	public var isAtEnd: Bool {
		return self.currentIndex >= self.count!
	}

	public mutating func decodeNil() throws -> Bool {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(Any?.self, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		if self.container[self.currentIndex] is NSNull {
			self.currentIndex += 1
			return true
		} else {
			return false
		}
	}

	public mutating func decode(_ type: Bool.Type) throws -> Bool {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Bool.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: Int.Type) throws -> Int {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: Int8.Type) throws -> Int8 {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int8.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: Int16.Type) throws -> Int16 {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int16.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: Int32.Type) throws -> Int32 {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int32.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: Int64.Type) throws -> Int64 {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Int64.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: UInt.Type) throws -> UInt {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: UInt8.Type) throws -> UInt8 {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt8.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: UInt16.Type) throws -> UInt16 {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt16.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: UInt32.Type) throws -> UInt32 {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt32.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: UInt64.Type) throws -> UInt64 {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: UInt64.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: Float.Type) throws -> Float {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Float.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: Double.Type) throws -> Double {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: Double.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode(_ type: String.Type) throws -> String {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: String.self) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func decode<T : Decodable>(_ type: T.Type) throws -> T {
		tracker.use(currentIndex)
		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Unkeyed container is at end."))
		}

		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard let decoded = try self.decoder.unbox(self.container[self.currentIndex], as: type) else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.decoder.codingPath + [_JSONKey(index: self.currentIndex)], debugDescription: "Expected \(type) but found null instead."))
		}

		self.currentIndex += 1
		return decoded
	}

	public mutating func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type) throws -> KeyedDecodingContainer<NestedKey> {
		tracker.use(currentIndex)
		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(KeyedDecodingContainer<NestedKey>.self,
			                                  DecodingError.Context(codingPath: self.codingPath,
			                                                        debugDescription: "Cannot get nested keyed container -- unkeyed container is at end."))
		}

		let value = self.container[self.currentIndex]
		guard !(value is NSNull) else {
			throw DecodingError.valueNotFound(KeyedDecodingContainer<NestedKey>.self,
			                                  DecodingError.Context(codingPath: self.codingPath,
			                                                        debugDescription: "Cannot get keyed decoding container -- found null value instead."))
		}

		guard let dictionary = value as? [String : Any] else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: [String : Any].self, reality: value)
		}

		self.currentIndex += 1
		let container = _JSONKeyedDecodingContainer<NestedKey>(referencing: self.decoder, wrapping: dictionary)
		return KeyedDecodingContainer(container)
	}

	public mutating func nestedUnkeyedContainer() throws -> UnkeyedDecodingContainer {
		tracker.use(currentIndex)
		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(UnkeyedDecodingContainer.self,
			                                  DecodingError.Context(codingPath: self.codingPath,
			                                                        debugDescription: "Cannot get nested keyed container -- unkeyed container is at end."))
		}

		let value = self.container[self.currentIndex]
		guard !(value is NSNull) else {
			throw DecodingError.valueNotFound(UnkeyedDecodingContainer.self,
			                                  DecodingError.Context(codingPath: self.codingPath,
			                                                        debugDescription: "Cannot get keyed decoding container -- found null value instead."))
		}

		guard let array = value as? [Any] else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: [Any].self, reality: value)
		}

		self.currentIndex += 1
		return _JSONUnkeyedDecodingContainer(referencing: self.decoder, wrapping: array)
	}

	public mutating func superDecoder() throws -> Decoder {
		tracker.use(currentIndex)
		self.decoder.codingPath.append(_JSONKey(index: self.currentIndex))
		defer { self.decoder.codingPath.removeLast() }

		guard !self.isAtEnd else {
			throw DecodingError.valueNotFound(Decoder.self,
			                                  DecodingError.Context(codingPath: self.codingPath,
			                                                        debugDescription: "Cannot get superDecoder() -- unkeyed container is at end."))
		}

		let value = self.container[self.currentIndex]
		self.currentIndex += 1
		return _PedanticJSONDecoder(referencing: value, at: self.decoder.codingPath, options: self.decoder.options, tracker: decoder.tracker)
	}
}

extension _PedanticJSONDecoder : SingleValueDecodingContainer {
	// MARK: SingleValueDecodingContainer Methods

	private func expectNonNull<T>(_ type: T.Type) throws {
		guard !self.decodeNil() else {
			throw DecodingError.valueNotFound(type, DecodingError.Context(codingPath: self.codingPath, debugDescription: "Expected \(type) but found null value instead."))
		}
	}

	public func decodeNil() -> Bool {
		return self.storage.topContainer is NSNull
	}

	public func decode(_ type: Bool.Type) throws -> Bool {
		try expectNonNull(Bool.self)
		return try self.unbox(self.storage.topContainer, as: Bool.self)!
	}

	public func decode(_ type: Int.Type) throws -> Int {
		try expectNonNull(Int.self)
		return try self.unbox(self.storage.topContainer, as: Int.self)!
	}

	public func decode(_ type: Int8.Type) throws -> Int8 {
		try expectNonNull(Int8.self)
		return try self.unbox(self.storage.topContainer, as: Int8.self)!
	}

	public func decode(_ type: Int16.Type) throws -> Int16 {
		try expectNonNull(Int16.self)
		return try self.unbox(self.storage.topContainer, as: Int16.self)!
	}

	public func decode(_ type: Int32.Type) throws -> Int32 {
		try expectNonNull(Int32.self)
		return try self.unbox(self.storage.topContainer, as: Int32.self)!
	}

	public func decode(_ type: Int64.Type) throws -> Int64 {
		try expectNonNull(Int64.self)
		return try self.unbox(self.storage.topContainer, as: Int64.self)!
	}

	public func decode(_ type: UInt.Type) throws -> UInt {
		try expectNonNull(UInt.self)
		return try self.unbox(self.storage.topContainer, as: UInt.self)!
	}

	public func decode(_ type: UInt8.Type) throws -> UInt8 {
		try expectNonNull(UInt8.self)
		return try self.unbox(self.storage.topContainer, as: UInt8.self)!
	}

	public func decode(_ type: UInt16.Type) throws -> UInt16 {
		try expectNonNull(UInt16.self)
		return try self.unbox(self.storage.topContainer, as: UInt16.self)!
	}

	public func decode(_ type: UInt32.Type) throws -> UInt32 {
		try expectNonNull(UInt32.self)
		return try self.unbox(self.storage.topContainer, as: UInt32.self)!
	}

	public func decode(_ type: UInt64.Type) throws -> UInt64 {
		try expectNonNull(UInt64.self)
		return try self.unbox(self.storage.topContainer, as: UInt64.self)!
	}

	public func decode(_ type: Float.Type) throws -> Float {
		try expectNonNull(Float.self)
		return try self.unbox(self.storage.topContainer, as: Float.self)!
	}

	public func decode(_ type: Double.Type) throws -> Double {
		try expectNonNull(Double.self)
		return try self.unbox(self.storage.topContainer, as: Double.self)!
	}

	public func decode(_ type: String.Type) throws -> String {
		try expectNonNull(String.self)
		return try self.unbox(self.storage.topContainer, as: String.self)!
	}

	public func decode<T : Decodable>(_ type: T.Type) throws -> T {
		try expectNonNull(type)
		return try self.unbox(self.storage.topContainer, as: type)!
	}
}

// MARK: - Concrete Value Representations

extension _PedanticJSONDecoder {
	/// Returns the given value unboxed from a container.
	fileprivate func unbox(_ value: Any, as type: Bool.Type) throws -> Bool? {
		guard !(value is NSNull) else { return nil }

		#if DEPLOYMENT_RUNTIME_SWIFT
		// Bridging differences require us to split implementations here
		guard let number = value as? NSNumber else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		// TODO: Add a flag to coerce non-boolean numbers into Bools?
		guard number._cfTypeID == CFBooleanGetTypeID() else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		return number.boolValue
		#else
		if let number = value as? NSNumber {
			// TODO: Add a flag to coerce non-boolean numbers into Bools?
			if number === kCFBooleanTrue as NSNumber {
				return true
			} else if number === kCFBooleanFalse as NSNumber {
				return false
			}

			/* FIXME: If swift-corelibs-foundation doesn't change to use NSNumber, this code path will need to be included and tested:
			 } else if let bool = value as? Bool {
			 return bool
			 */

		}

		throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		#endif
	}

	fileprivate func unbox(_ value: Any, as type: Int.Type) throws -> Int? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let int = number.intValue
		guard NSNumber(value: int) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return int
	}

	fileprivate func unbox(_ value: Any, as type: Int8.Type) throws -> Int8? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let int8 = number.int8Value
		guard NSNumber(value: int8) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return int8
	}

	fileprivate func unbox(_ value: Any, as type: Int16.Type) throws -> Int16? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let int16 = number.int16Value
		guard NSNumber(value: int16) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return int16
	}

	fileprivate func unbox(_ value: Any, as type: Int32.Type) throws -> Int32? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let int32 = number.int32Value
		guard NSNumber(value: int32) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return int32
	}

	fileprivate func unbox(_ value: Any, as type: Int64.Type) throws -> Int64? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let int64 = number.int64Value
		guard NSNumber(value: int64) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return int64
	}

	fileprivate func unbox(_ value: Any, as type: UInt.Type) throws -> UInt? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let uint = number.uintValue
		guard NSNumber(value: uint) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return uint
	}

	fileprivate func unbox(_ value: Any, as type: UInt8.Type) throws -> UInt8? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let uint8 = number.uint8Value
		guard NSNumber(value: uint8) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return uint8
	}

	fileprivate func unbox(_ value: Any, as type: UInt16.Type) throws -> UInt16? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let uint16 = number.uint16Value
		guard NSNumber(value: uint16) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return uint16
	}

	fileprivate func unbox(_ value: Any, as type: UInt32.Type) throws -> UInt32? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let uint32 = number.uint32Value
		guard NSNumber(value: uint32) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return uint32
	}

	fileprivate func unbox(_ value: Any, as type: UInt64.Type) throws -> UInt64? {
		guard !(value is NSNull) else { return nil }

		guard let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		let uint64 = number.uint64Value
		guard NSNumber(value: uint64) == number else {
			throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number <\(number)> does not fit in \(type)."))
		}

		return uint64
	}

	fileprivate func unbox(_ value: Any, as type: Float.Type) throws -> Float? {
		guard !(value is NSNull) else { return nil }

		if let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse {
			// We are willing to return a Float by losing precision:
			// * If the original value was integral,
			//   * and the integral value was > Float.greatestFiniteMagnitude, we will fail
			//   * and the integral value was <= Float.greatestFiniteMagnitude, we are willing to lose precision past 2^24
			// * If it was a Float, you will get back the precise value
			// * If it was a Double or Decimal, you will get back the nearest approximation if it will fit
			let double = number.doubleValue
			guard abs(double) <= Double(Float.greatestFiniteMagnitude) else {
				throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Parsed JSON number \(number) does not fit in \(type)."))
			}

			return Float(double)

			/* FIXME: If swift-corelibs-foundation doesn't change to use NSNumber, this code path will need to be included and tested:
		} else if let double = value as? Double {
			if abs(double) <= Double(Float.max) {
				return Float(double)
			}
			overflow = true
		} else if let int = value as? Int {
			if let float = Float(exactly: int) {
				return float
			}

			 overflow = true
			 */

		} else if let string = value as? String,
			case .convertFromString(let posInfString, let negInfString, let nanString) = self.options.nonConformingFloatDecodingStrategy {
			if string == posInfString {
				return Float.infinity
			} else if string == negInfString {
				return -Float.infinity
			} else if string == nanString {
				return Float.nan
			}
		}

		throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
	}

	fileprivate func unbox(_ value: Any, as type: Double.Type) throws -> Double? {
		guard !(value is NSNull) else { return nil }

		if let number = value as? NSNumber, number !== kCFBooleanTrue, number !== kCFBooleanFalse {
			// We are always willing to return the number as a Double:
			// * If the original value was integral, it is guaranteed to fit in a Double; we are willing to lose precision past 2^53 if you encoded a UInt64 but requested a Double
			// * If it was a Float or Double, you will get back the precise value
			// * If it was Decimal, you will get back the nearest approximation
			return number.doubleValue

			/* FIXME: If swift-corelibs-foundation doesn't change to use NSNumber, this code path will need to be included and tested:
			} else if let double = value as? Double {
				return double
			} else if let int = value as? Int {
				if let double = Double(exactly: int) {
					return double
				 }

			 overflow = true
			 */

		} else if let string = value as? String,
			case .convertFromString(let posInfString, let negInfString, let nanString) = self.options.nonConformingFloatDecodingStrategy {
			if string == posInfString {
				return Double.infinity
			} else if string == negInfString {
				return -Double.infinity
			} else if string == nanString {
				return Double.nan
			}
		}

		throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
	}

	fileprivate func unbox(_ value: Any, as type: String.Type) throws -> String? {
		guard !(value is NSNull) else { return nil }

		guard let string = value as? String else {
			throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
		}

		return string
	}

	fileprivate func unbox(_ value: Any, as type: Date.Type) throws -> Date? {
		guard !(value is NSNull) else { return nil }

		switch self.options.dateDecodingStrategy {
		case .deferredToDate:
			self.storage.push(container: value)
			defer { self.storage.popContainer() }
			return try Date(from: self)

		case .secondsSince1970:
			let double = try self.unbox(value, as: Double.self)!
			return Date(timeIntervalSince1970: double)

		case .millisecondsSince1970:
			let double = try self.unbox(value, as: Double.self)!
			return Date(timeIntervalSince1970: double / 1000.0)

		case .iso8601:
			if #available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
				let string = try self.unbox(value, as: String.self)!
				guard let date = _iso8601Formatter.date(from: string) else {
					throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Expected date string to be ISO8601-formatted."))
				}

				return date
			} else {
				fatalError("ISO8601DateFormatter is unavailable on this platform.")
			}

		case .formatted(let formatter):
			let string = try self.unbox(value, as: String.self)!
			guard let date = formatter.date(from: string) else {
				throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Date string does not match format expected by formatter."))
			}

			return date

		case .custom(let closure):
			self.storage.push(container: value)
			defer { self.storage.popContainer() }
			return try closure(self)
		}
	}

	fileprivate func unbox(_ value: Any, as type: Data.Type) throws -> Data? {
		guard !(value is NSNull) else { return nil }

		switch self.options.dataDecodingStrategy {
		case .deferredToData:
			self.storage.push(container: value)
			defer { self.storage.popContainer() }
			return try Data(from: self)

		case .base64:
			guard let string = value as? String else {
				throw makeTypeMismatchError(at: self.codingPath, expectation: type, reality: value)
			}

			guard let data = Data(base64Encoded: string) else {
				throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath, debugDescription: "Encountered Data is not valid Base64."))
			}

			return data

		case .custom(let closure):
			self.storage.push(container: value)
			defer { self.storage.popContainer() }
			return try closure(self)
		}
	}

	fileprivate func unbox(_ value: Any, as type: Decimal.Type) throws -> Decimal? {
		guard !(value is NSNull) else { return nil }

		// Attempt to bridge from NSDecimalNumber.
		if let decimal = value as? Decimal {
			return decimal
		} else {
			let doubleValue = try self.unbox(value, as: Double.self)!
			return Decimal(doubleValue)
		}
	}

	fileprivate func unbox<T : Decodable>(_ value: Any, as type: T.Type) throws -> T? {
		let decoded: T
		#if DEPLOYMENT_RUNTIME_SWIFT
		// Bridging differences require us to split implementations here
		if T.self == Date.self {
			guard let date = try self.unbox(value, as: Date.self) else { return nil }
			decoded = date as! T
		} else if T.self == Data.self {
			guard let data = try self.unbox(value, as: Data.self) else { return nil }
			decoded = data as! T
		} else if T.self == URL.self {
			guard let urlString = try self.unbox(value, as: String.self) else {
				return nil
			}

			guard let url = URL(string: urlString) else {
				throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
				                                                        debugDescription: "Invalid URL string."))
			}
			decoded = (url as! T)
		} else if T.self == Decimal.self {
			guard let decimal = try self.unbox(value, as: Decimal.self) else { return nil }
			decoded = decimal as! T
		} else {
			self.storage.push(container: value)
			defer { self.storage.popContainer() }
			return try type.init(from: self)
		}
		return decoded
		#else
		if type == Date.self || type == NSDate.self {
			return try self.unbox(value, as: Date.self) as? T
		} else if type == Data.self || type == NSData.self {
			return try self.unbox(value, as: Data.self) as? T
		} else if type == URL.self || type == NSURL.self {
			guard let urlString = try self.unbox(value, as: String.self) else {
				return nil
			}

			guard let url = URL(string: urlString) else {
				throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: self.codingPath,
				                                                        debugDescription: "Invalid URL string."))
			}

			return (url as! T)
		} else if type == Decimal.self || type == NSDecimalNumber.self {
			return try self.unbox(value, as: Decimal.self) as? T
		} else {
			self.storage.push(container: value)
			defer { self.storage.popContainer() }
			return try type.init(from: self)
		}
		#endif
	}
}

//===----------------------------------------------------------------------===//
// Shared Key Types
//===----------------------------------------------------------------------===//

fileprivate struct _JSONKey : CodingKey {
	public var stringValue: String
	public var intValue: Int?

	public init?(stringValue: String) {
		self.stringValue = stringValue
		self.intValue = nil
	}

	public init?(intValue: Int) {
		self.stringValue = "\(intValue)"
		self.intValue = intValue
	}

	public init(stringValue: String, intValue: Int?) {
		self.stringValue = stringValue
		self.intValue = intValue
	}
	fileprivate init(index: Int) {
		self.stringValue = "Index \(index)"
		self.intValue = index
	}

	fileprivate static let `super` = _JSONKey(stringValue: "super")!
}

//===----------------------------------------------------------------------===//
// Shared ISO8601 Date Formatter
//===----------------------------------------------------------------------===//

// NOTE: This value is implicitly lazy and _must_ be lazy. We're compiled against the latest SDK (w/ ISO8601DateFormatter), but linked against whichever Foundation the user has. ISO8601DateFormatter might not exist, so we better not hit this code path on an older OS.
@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
fileprivate var _iso8601Formatter: ISO8601DateFormatter = {
	let formatter = ISO8601DateFormatter()
	formatter.formatOptions = .withInternetDateTime
	return formatter
}()
