export let standardLibSource = `
# FIXME add more stuff!
# boolean operators
export external operator or(left: boolean, right: boolean): boolean;
export external operator and(left: boolean, right: boolean): boolean;
export external operator xor(left: boolean, right: boolean): boolean;

# int8 functions  & operators
export external func int8(value: int8): int8;
export external func int8(value: int16): int8;
export external func int8(value: int32): int8;
export external func int8(value: int64): int8;
export external func int8(value: float32): int8;
export external func int8(value: float64): int8;
export external operator <(left: int8, right: int8): boolean;
export external operator <=(left: int8, right: int8): boolean;
export external operator >(left: int8, right: int8): boolean;
export external operator >=(left: int8, right: int8): boolean;
export external operator +(left: int8, right: int8): int8;
export external operator -(left: int8, right: int8): int8;
export external operator /(left: int8, right: int8): int8;
export external operator *(left: int8, right: int8): int8;
export external operator %(left: int8, right: int8): int8;

# int16 functions & operators
export external func int16(value: int8): int16;
export external func int16(value: int16): int16;
export external func int16(value: int32): int16;
export external func int16(value: int64): int16;
export external func int16(value: float32): int16;
export external func int16(value: float64): int16;

export external operator <(left: int16, right: int16): boolean;
export external operator <=(left: int16, right: int16): boolean;
export external operator >(left: int16, right: int16): boolean;
export external operator >=(left: int16, right: int16): boolean;
export external operator +(left: int16, right: int16): int16;
export external operator -(left: int16, right: int16): int16;
export external operator /(left: int16, right: int16): int16;
export external operator *(left: int16, right: int16): int16;
export external operator %(left: int16, right: int16): int16;

# int32 functions & operators
export external func int32(value: int8): int32;
export external func int32(value: int16): int32;
export external func int32(value: int32): int32;
export external func int32(value: int64): int32;
export external func int32(value: float32): int32;
export external func int32(value: float64): int32;

export external operator <(left: int32, right: int32): boolean;
export external operator <=(left: int32, right: int32): boolean;
export external operator >(left: int32, right: int32): boolean;
export external operator >=(left: int32, right: int32): boolean;
export external operator +(left: int32, right: int32): int32;
export external operator -(left: int32, right: int32): int32;
export external operator /(left: int32, right: int32): int32;
export external operator *(left: int32, right: int32): int32;
export external operator %(left: int32, right: int32): int32;

# float32 functions & operators
export external func float32(value: int8): float32;
export external func float32(value: int16): float32;
export external func float32(value: int32): float32;
export external func float32(value: int64): float32;
export external func float32(value: float32): float32;
export external func float32(value: float64): float32;

export external operator <(left: float32, right: float32): boolean;
export external operator <=(left: float32, right: float32): boolean;
export external operator >(left: float32, right: float32): boolean;
export external operator >=(left: float32, right: float32): boolean;
export external operator +(left: float32, right: float32): float32;
export external operator -(left: float32, right: float32): float32;
export external operator /(left: float32, right: float32): float32;
export external operator *(left: float32, right: float32): float32;
export external operator %(left: float32, right: float32): float32;

# number/float64 functions & operators
export external func float64(value: int8): float64;
export external func float64(value: int16): float64;
export external func float64(value: int32): float64;
export external func float64(value: int64): float64;
export external func float64(value: float32): float64;
export external func float64(value: float64): float64;

export external func number(value: int8): number;
export external func number(value: int16): number;
export external func number(value: int32): number;
export external func number(value: int64): number;
export external func number(value: float32): number;
export external func number(value: float64): number;

export external operator <(left: number, right: number): boolean;
export external operator <=(left: number, right: number): boolean;
export external operator >(left: number, right: number): boolean;
export external operator >=(left: number, right: number): boolean;
export external operator +(left: number, right: number): number;
export external operator -(left: number, right: number): number;
export external operator /(left: number, right: number): number;
export external operator *(left: number, right: number): number;
export external operator %(left: number, right: number): number;

# String functions & operators
export external func length(str: string): number;
export external func toString(n: nothing): string;
export external func toString(n: number): string;
export external func toString(n: boolean): string;
export external func charAt(str: string, index: number): string;
export external operator [](str: string, index: number): string;
export external operator +(left: string, right: string): string;

# List functions
export external func length[T](list: [T]): number;
export external func push[T](list: [T], element: T): nothing;
export external func push[T](list: [T], elements: [T]): nothing;
export external func pop[T](list: [T]): T;
export external operator [][T](list: [T], index: number): T;
export external operator [][T](list: [T], index: number, element: T): T;

# Map functions
export external func length[T](map: {T}): number;
export external func set[T](map: {T}, key: string, value: T): nothing;
export external func has[T](map: {T}, key: string): boolean;
export external func get[T](map: {T}, key: string): T | nothing;
export external func get[T](map: {T}, key: string, default: T): T;
export external operator [][T](map: {T}, key: string): T;
export external operator [][T](map: {T}, key: string, element: T): T;

# Basic IO
export external func print(message: string): nothing;
export external func print(num: boolean): nothing;
export external func print(num: number): nothing;
`;
// standardLibSource = "";
