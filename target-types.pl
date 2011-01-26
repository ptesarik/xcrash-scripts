#! /usr/bin/perl

while (<>) {
    s/\bshort unsigned int\b/tushort/g;
    s/\bunsigned long long\b/tulonglong/g;
    s/\bunsigned int\b/tuint/g;
    s/\blong long\b/tlonglong/g;
    s/\bunsigned long\b/tulonglong/g;
    s/\bshort\b/tshort/g;
    s/\bushort\b/tushort/g;
    s/\bint\b/tint/g;
    s/\buint\b/tuint/g;
    s/\blong\b/tlong/g;
    s/\bulong\b/tulong/g;
    s/\blonglong\b/tlonglong/g;
    s/\bulonglong\b/tulonglong/g;
    print;
}
