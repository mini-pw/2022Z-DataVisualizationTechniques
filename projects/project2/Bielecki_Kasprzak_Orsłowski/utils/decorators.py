def all_args_none(default_val):
    """
    A decorator. It makes decorated function return specified value if all
    passed arguments are None.

    :param default_val: a value to be returned when all arguments
        passed to a decorated function are None
    """

    def inner(func):
        def wrapper(*args, **kwargs):
            if all((arg is None for arg in args)) and \
                    all(kwarg is None for kwarg in kwargs.values()):
                return default_val
            return func(*args, **kwargs)

        return wrapper

    return inner
