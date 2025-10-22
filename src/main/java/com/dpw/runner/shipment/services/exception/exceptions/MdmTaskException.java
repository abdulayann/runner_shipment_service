package com.dpw.runner.shipment.services.exception.exceptions;


public class MdmTaskException extends RuntimeException {
    public MdmTaskException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public MdmTaskException(String msg) {
        super(msg);
    }
}
