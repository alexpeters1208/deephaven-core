//
// Copyright (c) 2016-2025 Deephaven Data Labs and Patent Pending
//
package io.deephaven.plot.util.functions;

import groovy.lang.Closure;

/**
 * Wraps a {@link HasClosure} with the API of a function.
 */
public class ClosureFunction<T, R> extends HasClosure<R> implements SerializableFunction<T, R> {

    private static final long serialVersionUID = 3693316124178311688L;

    /**
     * Creates a ClosureFunction instance with the {@code closure}.
     *
     * @param closure closure
     */
    public ClosureFunction(final Closure<R> closure) {
        super(closure);
    }

    @Override
    public R apply(T t) {
        return (R) getClosure().call(t);
    }
}
