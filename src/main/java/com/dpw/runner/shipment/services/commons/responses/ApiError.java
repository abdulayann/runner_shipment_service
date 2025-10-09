package com.dpw.runner.shipment.services.commons.responses;


import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import org.springframework.http.HttpStatus;

import java.io.Serializable;
import java.util.List;

@SuppressWarnings("unused")
@JsonPropertyOrder({ "status", "message", "errors" })
@Schema(description = "Api Error Model")
public class ApiError implements Serializable {

    @Schema(name = "status")
    private HttpStatus status;

    @Schema(name = "message")
    private String message;

    @Schema(name = "errors")
    private List<String> errors;

    public ApiError() {
        super();
    }

    public ApiError(final HttpStatus status, final String message, final List<String> errors) {
        super();
        this.status = status;
        this.message = message;
        this.errors = errors;
    }

    public ApiError(final HttpStatus status, final String message, final String error) {
        super();
        this.status = status;
        this.message = message;
        errors = List.of(error);
    }

    public ApiError(final HttpStatus status, final String message) {
        super();
        this.status = status;
        this.message = message;
        errors = null;
    }

    public ApiError(final HttpStatus status) {
        super();
        this.status = status;
    }

    public HttpStatus getStatus() {
        return status;
    }

    public void setStatus(final HttpStatus status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(final String message) {
        this.message = message;
    }

    public List<String> getErrors() {
        return errors;
    }

    public void setErrors(final List<String> errors) {
        this.errors = errors;
    }

    public void setError(final String error) {
        errors = List.of(error);
    }
}
