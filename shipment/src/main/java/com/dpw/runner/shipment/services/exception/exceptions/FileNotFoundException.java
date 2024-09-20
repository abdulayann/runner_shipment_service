package com.dpw.runner.shipment.services.exception.exceptions;

import com.dpw.runner.shipment.services.utils.Generated;

/**
 * Use this exception when File not found 
 */
@Generated
public class FileNotFoundException extends RuntimeException {

    public FileNotFoundException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public FileNotFoundException(String msg) {
        super(msg);
    }
}
