package com.dpw.runner.booking.services.exception.exceptions;

import com.dpw.runner.booking.services.utils.Generated;

/**
 * Use this exception when File not found 
 */
@Generated
public class MandatoryFieldException extends RuntimeException {

    public MandatoryFieldException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public MandatoryFieldException(String msg) {
        super(msg);
    }
}
