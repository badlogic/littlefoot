const libSource = `
export external operator or(left: boolean, right: boolean): boolean;
export external operator and(left: boolean, right: boolean): boolean;
export external operator xor(left: boolean, right: boolean): boolean;
export external operator <(left: number, right: number): boolean;
export external operator <=(left: number, right: number): boolean;
export external operator >(left: number, right: number): boolean;
export external operator >=(left: number, right: number): boolean;
export external operator +(left: number, right: number): number;
export external operator +(left: string, right: string): string;
export external operator -(left: number, right: number): number;
export external operator /(left: number, right: number): number;
export external operator *(left: number, right: number): number;
export external operator %(left: number, right: number): number;

# List functions
export external func length[T](list: [T]): number;
export external func push[T](list: [T], element: T): nothing;
export external func push[T](list: [T], elements: [T]): nothing;
export external func pop[T](list: [T]): T;
export external operator [][T](list: [T], index: number): T;
export external operator [][T](list: [T], element: T): T;

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
`;
