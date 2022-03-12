#include <iostream>
#include <string>
#include <fstream>

using namespace std;

typedef unsigned char BASE;
#define BASE_SIZE (sizeof(BASE)*8)

constexpr char alphabet[] = "0123456789abcdef";

class BN {
    BASE* coef;
    int len;
    int capacity;
public:
    BN(int maxlen = 1, bool fill_zeros = true) {
        capacity = maxlen;
        len = 0;
        coef = new BASE[capacity];
        if (fill_zeros)
            for (int i = 0; i < capacity; i++)
                coef[i] = 0;
        else
            for (int i = 0; i < capacity; i++) {
                coef[i] = rand();
            }
    }
    BN(const BN& other) {
        capacity = other.capacity;
        len = other.len;
        coef = new BASE[capacity];
        for (int i = 0; i < len; i++)
            coef[i] = other.coef[i];
    }
    BN& operator = (const BN& other) {
        if (this != &other) {
            if (capacity < other.capacity) capacity = other.capacity;
            len = other.len;
            delete[]coef;
            coef = new BASE[capacity];
            for (int i = 0; i < len; i++) {
                coef[i] = other.coef[i];
            }
        }
        return *this;
    }
    BN operator + (const BN& other) {
        const BN* term;
        BN result;
        if (len < other.len) {
            term = this;
            if (other.capacity - other.len < 2) result.capacity = other.capacity + 1;
            result = other;
        }
        else {
            term = &other;
            if (capacity - len < 2) result.capacity = capacity + 1;
            result = *this;
        }
        BASE carry = 0;
        int i;
        for (i = 0; i < term->len; i++) {
            result.coef[i] += (term->coef[i] + carry);
            if (result.coef[i] <= term->coef[i]) carry = 1;
            else carry = 0;
        }
        while (carry != 0) {
            result.coef[i] += carry;
            if (result.coef[i]) carry = 0;
            i++;
        }
        if (i > result.len) result.len = i;
        return result;
    }
    ~BN() { delete[]coef; coef = nullptr; }

    friend istream& operator >> (istream&, BN&);
    friend ostream& operator << (ostream&, const BN&);
};

istream& operator >> (istream& in, BN& self) {
    string line;
    char buffer;
    bool skip_zero = true;
    in >> noskipws >> buffer;
    while (buffer >= '0' && buffer <= '9' || buffer >= 'a' && buffer <= 'f') {
        buffer >= '0' && buffer <= '9' ? buffer -= '0' : buffer = buffer - 'a' + 10;
        if (!buffer && skip_zero) {
            in >> noskipws >> buffer;
            continue;
        }
        skip_zero = false;
        line += buffer;
        in >> noskipws >> buffer;
    }
    for (int low = 0, high = line.length() - 1; low <= high; low++, high--) {
        if (line[low] != line[high]) {
            line[low] ^= line[high];
            line[high] ^= line[low];
            line[low] ^= line[high];
        }
    }
    self.capacity = line.length() * 4 / BASE_SIZE;
    if (line.length() * 4 % BASE_SIZE) self.capacity++;
    self.len = self.capacity;
    delete[]self.coef;
    self.coef = new BASE[self.capacity];
    int current_element = 0;
    self.coef[current_element] = 0;
    const int element_capacity = BASE_SIZE / 4;
    int base_counter = 0;
    for (int i = 0; i < line.length(); i++) {
        self.coef[current_element] += line[i] << (4 * base_counter);
        base_counter++;
        if (base_counter == element_capacity) {
            base_counter = 0;
            current_element++;
            self.coef[current_element] = 0;
        }
    }

    return in;
}
ostream& operator << (ostream& out, const BN& self) {
    out << "0x";
    bool skip_zero = true;
    for (int i = self.len - 1; i >= 0; i--) {
        if (i || self.coef[i] != 0)
            for (int j = BASE_SIZE - 4; j >= 0; j -= 4) {
                BASE temp = 15 & self.coef[i] >> j;
                if (!temp && skip_zero) continue;
                out << alphabet[temp];
                skip_zero = false;
            }
        else out << 0;
    }
    return out;
}


// 0ad3f06c50b16a11f54208fe19a17546773db52e9225d75bcfdb2614956ccfe9234537978e63dfc3c6857929a5f9e3fac33495a941df6753a53225331dc74113e5f6ccdf8ed9f98f4d541409101d605ea8ec9082c610293fe1cba6d4518df359681a4db49ed1bd29c3d77eaa5fd5234b62b7e58724dbfb187b7a0fc0cbbafbd6f95e2e0e5633da4192cb5ae4109ee46c1a638bd0f808b3c2b3a212f5f837f001

int main() {
    BN a, b;
    cin >> a >> b;
    cout << a << "+" << b << endl;
    BN c = a + b;
    cout << c;
    return 0;
}