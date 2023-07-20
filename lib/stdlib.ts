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
`;
