package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

/**
 This exception is thrown if the backup process for the V2 0th-day migration fails.
 */
@Generated
public class InttraFailureException extends RuntimeException{

    public InttraFailureException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public InttraFailureException(String msg) {
        super(msg);
    }
}
