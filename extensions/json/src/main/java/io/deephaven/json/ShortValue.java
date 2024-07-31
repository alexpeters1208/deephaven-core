//
// Copyright (c) 2016-2024 Deephaven Data Labs and Patent Pending
//
package io.deephaven.json;

import io.deephaven.annotations.BuildableStyle;
import org.immutables.value.Value.Default;
import org.immutables.value.Value.Immutable;

import java.util.Set;

/**
 * Processes a JSON value as an {@code short}.
 */
@Immutable
@BuildableStyle
public abstract class ShortValue extends ValueSingleValueBase<Short> {

    public static Builder builder() {
        return ImmutableShortValue.builder();
    }

    /**
     * The lenient short options. Allows missing and accepts {@link JsonValueTypes#intLike()}.
     *
     * @return the lenient short options
     */
    public static ShortValue lenient() {
        return builder()
                .allowedTypes(JsonValueTypes.intLike())
                .build();
    }

    /**
     * The standard short options. Allows missing and accepts {@link JsonValueTypes#intOrNull()}.
     *
     * @return the standard short options
     */
    public static ShortValue standard() {
        return builder().build();
    }

    /**
     * The strict short options. Disallows missing and accepts {@link JsonValueTypes#int_()}.
     *
     * @return the strict short options
     */
    public static ShortValue strict() {
        return builder()
                .allowMissing(false)
                .allowedTypes(JsonValueTypes.int_())
                .build();
    }

    /**
     * {@inheritDoc} Must be a subset of {@link JsonValueTypes#numberLike()}. By default is
     * {@link JsonValueTypes#intOrNull()}.
     */
    @Override
    @Default
    public Set<JsonValueTypes> allowedTypes() {
        return JsonValueTypes.intOrNull();
    }

    @Override
    final Set<JsonValueTypes> universe() {
        return JsonValueTypes.numberLike();
    }

    @Override
    public final <T> T walk(Visitor<T> visitor) {
        return visitor.visit(this);
    }

    public interface Builder extends BuilderSpecial<Short, ShortValue, Builder> {

        Builder onNull(short onNull);

        Builder onMissing(short onMissing);
    }
}
