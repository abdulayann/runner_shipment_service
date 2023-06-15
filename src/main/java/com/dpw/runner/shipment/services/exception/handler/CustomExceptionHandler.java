package com.dpw.runner.shipment.services.exception.handler;

import com.dpw.runner.shipment.services.exception.exceptions.FileNotFoundException;
import com.dpw.runner.shipment.services.exception.exceptions.InvalidAccessTokenException;
import com.dpw.runner.shipment.services.exception.exceptions.InvalidAuthenticationException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.response.ApiError;
import com.dpw.runner.shipment.services.exception.response.RunnerResponse;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.NoHandlerFoundException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

@ControllerAdvice
public class CustomExceptionHandler extends ResponseEntityExceptionHandler {

    @ExceptionHandler(InvalidAccessTokenException.class)
    public final ResponseEntity<Object> handleInvalidAccessTokenException(InvalidAccessTokenException ex) {
        RunnerResponse runnerResponse =
                new RunnerResponse(false, new ApiError(HttpStatus.INTERNAL_SERVER_ERROR, ex.getLocalizedMessage()));
        return new ResponseEntity(runnerResponse, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(FileNotFoundException.class)
    public final ResponseEntity<Object> handleFileNotFoundException(FileNotFoundException ex) {
        RunnerResponse runnerResponse =
                new RunnerResponse(false, new ApiError(HttpStatus.NOT_FOUND, ex.getLocalizedMessage()));
        return new ResponseEntity(runnerResponse, HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(RunnerException.class)
    public final ResponseEntity<Object> handleRunnerException(RunnerException ex) {
        RunnerResponse runnerResponse =
                new RunnerResponse(false, new ApiError(HttpStatus.INTERNAL_SERVER_ERROR, ex.getLocalizedMessage()));
        return new ResponseEntity(runnerResponse, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(InvalidAuthenticationException.class)
    public final ResponseEntity<Object> handleAuthenticationException(RunnerException ex) {
        RunnerResponse runnerResponse =
                new RunnerResponse(false, new ApiError(HttpStatus.FORBIDDEN, ex.getLocalizedMessage()));
        return new ResponseEntity(runnerResponse, HttpStatus.FORBIDDEN);
    }

    @Override
    protected ResponseEntity<Object> handleNoHandlerFoundException(
            NoHandlerFoundException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {

        return handleExceptionInternal(ex, "Path URL does not exist, Please check the URL", headers, status, request);
    }
}
