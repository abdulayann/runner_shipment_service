package com.dpw.runner.shipment.services.exception.exceptions;

/**
 * Use this exception when invalid token is passed
 */
public class InvalidAccessTokenException extends RuntimeException {

    public InvalidAccessTokenException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public InvalidAccessTokenException(String msg) {
        super(msg);
    }
}
