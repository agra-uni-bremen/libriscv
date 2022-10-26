int
mul(int a, int b)
{
	if (a <= 0)
		return 0;
	else if (a <= 1)
		return b;
	else
		return b + mul(a - 1, b);
}

int main(void) {
	int result = mul(100,10);

	/* copy result to register $a1 */
	asm volatile ("addi a1, %0, 0"
	              : /* no output operands */
	              : "r" (result)
	              : "a1");
}
