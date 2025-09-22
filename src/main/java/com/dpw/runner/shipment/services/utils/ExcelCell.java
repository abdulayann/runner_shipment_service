package com.dpw.runner.shipment.services.utils;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface ExcelCell {
    int order() default 999;
    String displayName() default "";
    String displayNameOverride() default "";
    boolean requiredInV3() default true;
}
