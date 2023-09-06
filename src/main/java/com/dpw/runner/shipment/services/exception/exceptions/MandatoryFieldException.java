package com.dpw.runner.shipment.services.exception.exceptions;

/**
 * Use this exception when File not found 
 */

public class MandatoryFieldException extends RuntimeException {

    public MandatoryFieldException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public MandatoryFieldException(String msg) {
        super(msg);
    }
}
