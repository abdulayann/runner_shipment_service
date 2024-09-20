package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

/**
 * Use this exception when invalid token is passed
 */
@Generated
public class InvalidAccessTokenException extends RuntimeException {

    public InvalidAccessTokenException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public InvalidAccessTokenException(String msg) {
        super(msg);
    }
}
