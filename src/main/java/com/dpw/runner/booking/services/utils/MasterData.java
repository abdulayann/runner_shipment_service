package com.dpw.runner.booking.services.utils;

import com.dpw.runner.booking.services.masterdata.enums.MasterDataType;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface MasterData {
    MasterDataType type();
    String cascade() default "";
}
