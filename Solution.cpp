#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <stdexcept>
#include <cassert>


class BigNum {
private:
    std::vector<int> digits;  // Stored in reverse order: index 0 = units digit
    bool isNegative;

    // Remove leading zeros and fix zero sign
    void normalize() {
        while (digits.size() > 1 && digits.back() == 0)
            digits.pop_back();
        if (digits.size() == 1 && digits[0] == 0)
            isNegative = false;
    }

    // Compare absolute values
    static int compareAbsolute(const BigNum& left, const BigNum& right) {
        if (left.digits.size() != right.digits.size())
            return left.digits.size() < right.digits.size() ? -1 : 1;
        for (int i = (int)left.digits.size() - 1; i >= 0; --i) {
            if (left.digits[i] != right.digits[i])
                return left.digits[i] < right.digits[i] ? -1 : 1;
        }
        return 0;
    }

    // Add absolute values
    static BigNum addAbsolute(const BigNum& a, const BigNum& b) {
        BigNum result;
        result.digits.clear();
        int carry = 0;
        size_t maxLen = std::max(a.digits.size(), b.digits.size());
        for (size_t i = 0; i < maxLen || carry; ++i) {
            int digitA = (i < a.digits.size()) ? a.digits[i] : 0;
            int digitB = (i < b.digits.size()) ? b.digits[i] : 0;
            int total = digitA + digitB + carry;
            result.digits.push_back(total % 10);
            carry = total / 10;
        }
        result.normalize();
        return result;
    }

    // Subtract absolute values (assumes |a| >= |b|)
    static BigNum subtractAbsolute(const BigNum& a, const BigNum& b) {
        BigNum result;
        result.digits.clear();
        int borrow = 0;
        for (size_t i = 0; i < a.digits.size(); ++i) {
            int digitA = a.digits[i];
            int digitB = (i < b.digits.size()) ? b.digits[i] : 0;
            int diff = digitA - digitB - borrow;
            if (diff < 0) {
                diff += 10;
                borrow = 1;
            } else {
                borrow = 0;
            }
            result.digits.push_back(diff);
        }
        result.normalize();
        return result;
    }

public:
    // Default constructor: zero
    BigNum() : isNegative(false) {
        digits = {0};
    }

    // Construct from long long
    BigNum(long long value) : isNegative(false) {
        if (value < 0) {
            isNegative = true;
            value = -value;
        }
        if (value == 0) {
            digits = {0};
            return;
        }
        digits.clear();
        while (value > 0) {
            digits.push_back(static_cast<int>(value % 10));
            value /= 10;
        }
    }

    // Construct from string (with optional sign)
    BigNum(const std::string& str) : isNegative(false) {
        if (str.empty()) throw std::invalid_argument("Empty string not allowed");

        size_t startIndex = 0;
        if (str[0] == '-') {
            isNegative = true;
            startIndex = 1;
        } else if (str[0] == '+') {
            startIndex = 1;
        }

        digits.clear();
        for (int i = (int)str.size() - 1; i >= (int)startIndex; --i) {
            char ch = str[i];
            if (ch < '0' || ch > '9')
                throw std::invalid_argument("Invalid digit in string");
            digits.push_back(ch - '0');
        }
        normalize();
    }

    // Unary minus
    BigNum operator-() const {
        BigNum temp = *this;
        if (!isZero()) temp.isNegative = !temp.isNegative;
        return temp;
    }

    // Addition
    BigNum operator+(const BigNum& other) const {
        BigNum result;
        if (isNegative == other.isNegative) {
            result = addAbsolute(*this, other);
            result.isNegative = isNegative;
        } else {
            int cmp = compareAbsolute(*this, other);
            if (cmp == 0) {
                result = BigNum(0);
            } else if (cmp > 0) {
                result = subtractAbsolute(*this, other);
                result.isNegative = isNegative;
            } else {
                result = subtractAbsolute(other, *this);
                result.isNegative = other.isNegative;
            }
        }
        result.normalize();
        return result;
    }

    // Subtraction
    BigNum operator-(const BigNum& other) const {
        return *this + (-other);
    }

    // Multiply by single digit (0–9)
    BigNum multiplyByDigit(int factor) const {
        if (factor == 0) return BigNum(0);
        if (factor == 1) return *this;

        BigNum result;
        result.digits.assign(digits.size() + 5, 0);
        long long carry = 0;
        for (size_t i = 0; i < digits.size(); ++i) {
            long long product = 1LL * digits[i] * factor + carry;
            result.digits[i] = static_cast<int>(product % 10);
            carry = product / 10;
        }
        size_t pos = digits.size();
        while (carry) {
            result.digits[pos++] = static_cast<int>(carry % 10);
            carry /= 10;
        }
        result.normalize();
        result.isNegative = isNegative;
        return result;
    }

    // Full multiplication
    BigNum operator*(const BigNum& other) const {
        BigNum result;
        result.isNegative = (isNegative != other.isNegative);
        result.digits.assign(digits.size() + other.digits.size(), 0);

        for (size_t i = 0; i < digits.size(); ++i) {
            long long carry = 0;
            for (size_t j = 0; j < other.digits.size() || carry; ++j) {
                long long current = result.digits[i + j] +
                                   1LL * digits[i] * (j < other.digits.size() ? other.digits[j] : 0) + carry;
                result.digits[i + j] = static_cast<int>(current % 10);
                carry = current / 10;
            }
        }
        result.normalize();
        return result;
    }

    // Division and modulus
    static std::pair<BigNum, BigNum> divideWithRemainder(const BigNum& dividend, const BigNum& divisor) {
        if (divisor.isZero()) throw std::runtime_error("Division by zero");

        BigNum absDividend = dividend.abs();
        BigNum absDivisor = divisor.abs();

        if (compareAbsolute(absDividend, absDivisor) < 0)
            return { BigNum(0), absDividend };

        BigNum quotient;
        quotient.digits.assign(absDividend.digits.size(), 0);
        BigNum current(0);

        for (int i = (int)absDividend.digits.size() - 1; i >= 0; --i) {
            current = current.multiplyByDigit(10);
            current = current + BigNum(absDividend.digits[i]);

            int low = 0, high = 9, digit = 0;
            while (low <= high) {
                int mid = (low + high) / 2;
                BigNum product = absDivisor.multiplyByDigit(mid);
                if (compareAbsolute(product, current) <= 0) {
                    digit = mid;
                    low = mid + 1;
                } else {
                    high = mid - 1;
                }
            }
            quotient.digits[i] = digit;
            if (digit != 0)
                current = current - absDivisor.multiplyByDigit(digit);
        }

        quotient.isNegative = (dividend.isNegative != divisor.isNegative);
        quotient.normalize();
        current.isNegative = false;
        return { quotient, current };
    }

    BigNum operator/(const BigNum& other) const {
        return divideWithRemainder(*this, other).first;
    }

    BigNum operator%(const BigNum& other) const {
        return divideWithRemainder(*this, other).second;
    }

    // Comparison operators
    bool operator<(const BigNum& other) const {
        if (isNegative != other.isNegative)
            return isNegative;
        int cmp = compareAbsolute(*this, other);
        return isNegative ? (cmp > 0) : (cmp < 0);
    }

    bool operator==(const BigNum& other) const {
        return isNegative == other.isNegative && digits == other.digits;
    }

    bool operator!=(const BigNum& other) const {
        return !(*this == other);
    }

    bool isZero() const {
        return digits.size() == 1 && digits[0] == 0;
    }

    // Convert to string
    std::string toString() const {
        if (isZero()) return "0";
        std::string s;
        if (isNegative) s += '-';
        for (int i = (int)digits.size() - 1; i >= 0; --i)
            s += char('0' + digits[i]);
        return s;
    }

    BigNum abs() const {
        BigNum temp = *this;
        temp.isNegative = false;
        return temp;
    }
};

// Rational Number: numerator / denominator
class Fraction {
private:
    BigNum numerator;
    BigNum denominator;

    // Greatest Common Divisor
    static BigNum gcd(BigNum a, BigNum b) {
        a = a.abs();
        b = b.abs();
        while (!b.isZero()) {
            BigNum remainder = a % b;
            a = b;
            b = remainder;
        }
        return a;
    }

    // Simplify the fraction
    void simplify() {
        if (denominator.isZero()) throw std::runtime_error("Denominator cannot be zero");
        if (numerator.isZero()) {
            denominator = BigNum(1);
            return;
        }
        BigNum common = gcd(numerator.abs(), denominator.abs());
        numerator = numerator / common;
        denominator = denominator / common;
        if (denominator < BigNum(0)) {
            numerator = -numerator;
            denominator = -denominator;
        }
    }

public:
    Fraction() : numerator(0), denominator(1) {}
    Fraction(const BigNum& n) : numerator(n), denominator(1) {}
    Fraction(const BigNum& n, const BigNum& d) : numerator(n), denominator(d) { simplify(); }

    Fraction operator+(const Fraction& other) const {
        BigNum newNumerator = numerator * other.denominator + other.numerator * denominator;
        BigNum newDenominator = denominator * other.denominator;
        return Fraction(newNumerator, newDenominator);
    }

    Fraction operator-(const Fraction& other) const {
        BigNum newNumerator = numerator * other.denominator - other.numerator * denominator;
        BigNum newDenominator = denominator * other.denominator;
        return Fraction(newNumerator, newDenominator);
    }

    Fraction operator*(const Fraction& other) const {
        BigNum newNumerator = numerator * other.numerator;
        BigNum newDenominator = denominator * other.denominator;
        return Fraction(newNumerator, newDenominator);
    }

    Fraction operator/(const Fraction& other) const {
        if (other.numerator.isZero()) throw std::runtime_error("Cannot divide by zero fraction");
        BigNum newNumerator = numerator * other.denominator;
        BigNum newDenominator = denominator * other.numerator;
        return Fraction(newNumerator, newDenominator);
    }

    bool operator<(const Fraction& other) const {
        return numerator * other.denominator < other.numerator * denominator;
    }

    bool operator>(const Fraction& other) const {
        return other < *this;
    }

    bool operator==(const Fraction& other) const {
        return numerator * other.denominator == other.numerator * denominator;
    }

    bool operator!=(const Fraction& other) const {
        return !(*this == other);
    }

    std::string toString() const {
        if (denominator == BigNum(1)) return numerator.toString();
        return numerator.toString() + "/" + denominator.toString();
    }

    bool isZero() const { return numerator.isZero(); }

    Fraction abs() const {
        return Fraction(numerator.abs(), denominator.abs());
    }
};

// Convert a string in given base to base-10 BigNum
BigNum convertFromBase(const std::string& numberString, int base) {
    if (base < 2 || base > 36)
        throw std::invalid_argument("Base out of range [2,36]");

    BigNum decimalValue(0);
    BigNum baseValue(base);

    for (char c : numberString) {
        int digit;
        if (c >= '0' && c <= '9') digit = c - '0';
        else if (c >= 'A' && c <= 'Z') digit = c - 'A' + 10;
        else if (c >= 'a' && c <= 'z') digit = c - 'a' + 10;
        else throw std::invalid_argument("Invalid character in number");

        if (digit >= base) throw std::invalid_argument("Digit exceeds base");

        decimalValue = decimalValue * baseValue + BigNum(digit);
    }
    return decimalValue;
}

// Solve Ax = b using Gaussian Elimination with partial pivoting
void solveLinearSystem(std::vector<std::vector<Fraction>>& coefficientMatrix, std::vector<Fraction>& solution) {
    int n = static_cast<int>(coefficientMatrix.size());

    // Forward elimination with partial pivoting
    for (int column = 0; column < n; ++column) {
        int pivotRow = column;
        for (int row = column + 1; row < n; ++row) {
            if (coefficientMatrix[row][column].abs() > coefficientMatrix[pivotRow][column].abs())
                pivotRow = row;
        }

        if (pivotRow != column)
            std::swap(coefficientMatrix[column], coefficientMatrix[pivotRow]);

        if (coefficientMatrix[column][column].isZero())
            throw std::runtime_error("Singular matrix detected");

        for (int row = column + 1; row < n; ++row) {
            Fraction eliminationFactor = coefficientMatrix[row][column] / coefficientMatrix[column][column];
            for (int k = column; k <= n; ++k) {
                coefficientMatrix[row][k] = coefficientMatrix[row][k] - eliminationFactor * coefficientMatrix[column][k];
            }
        }
    }

    // Back substitution
    solution.assign(n, Fraction(0));
    for (int i = n - 1; i >= 0; --i) {
        Fraction currentSum(0);
        for (int j = i + 1; j < n; ++j) {
            currentSum = currentSum + coefficientMatrix[i][j] * solution[j];
        }
        solution[i] = (coefficientMatrix[i][n] - currentSum) / coefficientMatrix[i][i];
    }
}

// Main driver
int main() {
    try {
        std::vector<std::pair<long long, std::string>> inputPoints(3);
        for (int i = 0; i < 3; ++i) {
            if (!(std::cin >> inputPoints[i].first >> inputPoints[i].second)) {
                std::cerr << "Input format: x y (3 lines)\n";
                return 1;
            }
        }

        std::vector<BigNum> yValues(3);
        for (int i = 0; i < 3; ++i) {
            yValues[i] = convertFromBase(inputPoints[i].second, 10);  // Assume base 10
        }

        // Set up augmented matrix for quadratic: a*x^2 + b*x + c = y
        std::vector<std::vector<Fraction>> augmentedMatrix(3, std::vector<Fraction>(4));
        for (int i = 0; i < 3; ++i) {
            BigNum x(inputPoints[i].first);
            BigNum xSquared = x * x;
            augmentedMatrix[i][0] = Fraction(xSquared);     // a coefficient
            augmentedMatrix[i][1] = Fraction(x);            // b coefficient
            augmentedMatrix[i][2] = Fraction(1);            // c coefficient
            augmentedMatrix[i][3] = Fraction(yValues[i]);   // RHS (y value)
        }

        std::vector<Fraction> solutionCoefficients;
        solveLinearSystem(augmentedMatrix, solutionCoefficients);

        std::cout << "a = " << solutionCoefficients[0].toString() << "\n";
        std::cout << "b = " << solutionCoefficients[1].toString() << "\n";
        std::cout << "c = " << solutionCoefficients[2].toString() << "\n";

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
