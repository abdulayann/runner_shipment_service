package com.dpw.runner.shipment.services.exception.exceptions;

/**
 * Use this exception when File not found 
 */

public class FileNotFoundException extends RuntimeException {

    public FileNotFoundException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public FileNotFoundException(String msg) {
        super(msg);
    }
}
