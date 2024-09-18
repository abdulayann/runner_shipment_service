package com.dpw.runner.shipment.services.utils.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface StringModifier {
    int maxLength() default Integer.MAX_VALUE;
    String regex() default "";

    PatternType pattern();

    public enum PatternType {
        TEXT("[^a-zA-Z0-9\\s./\\\\-]"), ALPHA_NUMERIC("[^a-zA-Z0-9]"), ALPHA("[^a-zA-Z]");

        public final String patternString;

       public String patternString() {
           return patternString();
       }

       PatternType(String patternString) {
           this.patternString = patternString;
       }
    }
}


