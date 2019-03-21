import XCTest
@testable import PedanticJSONDecoder

struct A: Codable {
	var a: Bool
	var b: String
}

struct B: Codable {
	var a: Bool
	var b: String
	var c: Int
}

struct C: Codable {
	var a: Bool
	var b: String
	var c: Int?
}

struct D<Inner: Codable>: Codable {
	var a: Inner
	var b: Inner
}


var aJSON = """
	{
		"a": true,
		"b": "a"
	}
	"""

var bJSON = """
	{
		"a": true,
		"b": "a",
		"c": 4
	}
	"""

var aArrayJSON = "[\(aJSON), \(aJSON), \(aJSON)]"
var bArrayJSON = "[\(bJSON), \(bJSON), \(bJSON)]"
var abArrayJSON = "[\(aJSON), \(bJSON), \(aJSON)]"
var adJSON = "{\"a\": \(aJSON), \"b\": \(aJSON)}"
var bdJSON = "{\"a\": \(bJSON), \"b\": \(bJSON)}"

final class PedanticJSONDecoderTests: XCTestCase {
	func testNoThrowing() {
		let decoder = PedanticJSONDecoder()
		XCTAssertNoThrow(try decoder.decode(A.self, from: Data(aJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode(B.self, from: Data(bJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode([A].self, from: Data(aArrayJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode([B].self, from: Data(bArrayJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode(D<A>.self, from: Data(adJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode(D<B>.self, from: Data(bdJSON.utf8)))

		XCTAssertNoThrow(try decoder.decode(C.self, from: Data(aJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode(C.self, from: Data(bJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode([C].self, from: Data(aArrayJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode([C].self, from: Data(bArrayJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode(D<C>.self, from: Data(adJSON.utf8)))
		XCTAssertNoThrow(try decoder.decode(D<C>.self, from: Data(bdJSON.utf8)))
	}

	func testThrowing() {
		let decoder = PedanticJSONDecoder()
		XCTAssertThrowsError(try decoder.decode(A.self, from: Data(bJSON.utf8)))
		XCTAssertThrowsError(try decoder.decode(B.self, from: Data(aJSON.utf8)))
		XCTAssertThrowsError(try decoder.decode([A].self, from: Data(bArrayJSON.utf8)))
		XCTAssertThrowsError(try decoder.decode([B].self, from: Data(aArrayJSON.utf8)))
		XCTAssertThrowsError(try decoder.decode([A].self, from: Data(abArrayJSON.utf8)))
		XCTAssertThrowsError(try decoder.decode([B].self, from: Data(abArrayJSON.utf8)))
		XCTAssertThrowsError(try decoder.decode(D<A>.self, from: Data(bdJSON.utf8)))
		XCTAssertThrowsError(try decoder.decode(D<B>.self, from: Data(adJSON.utf8)))
	}

	static var allTests = [
		("Test valid JSON decoding", testNoThrowing),
		("Test invalid JSON decoding", testThrowing),
	]
}
