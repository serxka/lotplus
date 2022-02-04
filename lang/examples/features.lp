#[
############################
          CASTING
############################
]#


#[
############################
        ENUM CASTING
############################
]#


#[
############################
      MULTIPLE RETURN
############################
]#
# An enum where the 0 value marks a success and a 1 is an error
enum Error {
	Success,
	Failure;
	
	fn(void): void {
		
	}
}

# This function `foo` takes no input and returns two values, a pair in the
# type of <usize, Error>. A multiple return is marked by a type list wrapped
# in arrow brackets `<type1, type2, ...>`
foo(void): <usize, Error> {
	# To return the multiple values return can take a list of expression
	# separated by comma's
	ret 69, null;
}

# When we call a function with multiple return we must destructure it straight
# away to get the values. We cannot take a multiple return function and assign
# it to a single variable. There is no tuple type.
#
# We mark the separate variables by using a comma we would then insert the set
# symbol (`=`).
num:, err: = foo();

# We can also specify the type if we choose too. We do this the same as normal.
num: usize, err: Error = foo();
# We don't have to specify all the types, a single type is fine.
num: usize, err: = foo();
# As these are normal variables we are also allowed to list qualifiers.
num const:, err: = foo();

# Sometime we might not care about the return of both values. In this case we
# can replace the variable name with a single underline. Now only var is bound
num:, _: = foo();
_:, err: = foo();
_:, _: = foo();

# This alone is quite useful, however it's not uncommon that we want to call a
# function and then check to see it had an error. We can embed the function
# call into an if statement and perform error checking there. Below is an
# example of this within another calling function.
bar(void): <usize, Error> {
	# Just check for the error and propagate it upwards
	if (_:, e: = foo(); e != Success)
		ret null, e;

	# Call the function and check the error later, in this case if don't return
	# we can then still use `v` later.
	v:, e: = foo();
	if (e != Success)
		ret null, e;
	io.println("my num: %d", v); # Would be printed if there was no error

	# Call the function and check for an error now, proceeding to do something
	# given that there was no error.
	if (v:, e: = foo(); e == Success) {
		io.println("my num %d", v);
		# do other stuff with `v`
	}
}
