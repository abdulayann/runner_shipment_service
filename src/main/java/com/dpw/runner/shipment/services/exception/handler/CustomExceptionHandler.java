package com.dpw.runner.shipment.services.exception.handler;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.FileNotFoundException;
import com.dpw.runner.shipment.services.exception.exceptions.InvalidAccessTokenException;
import com.dpw.runner.shipment.services.exception.exceptions.InvalidAuthenticationException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.support.DefaultMessageSourceResolvable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.NoHandlerFoundException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import java.util.List;

@ControllerAdvice
@Slf4j
@Generated
public class CustomExceptionHandler extends ResponseEntityExceptionHandler {

    @ExceptionHandler({
            BillingException.class})
    private ResponseEntity<IRunnerResponse> handleBillingExceptions(final RuntimeException ex) {
        return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(AccessDeniedException.class)
    protected ResponseEntity<IRunnerResponse> handleAccessDeniedException(AccessDeniedException ex) {
        return ResponseHelper.buildFailedResponse("Authorization has been denied for this request.", HttpStatus.FORBIDDEN);
    }

    @ExceptionHandler(InvalidAccessTokenException.class)
    public final ResponseEntity<IRunnerResponse> handleInvalidAccessTokenException(InvalidAccessTokenException ex) {
        return ResponseHelper.buildFailedResponse(ex.getLocalizedMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(FileNotFoundException.class)
    public final ResponseEntity<IRunnerResponse> handleFileNotFoundException(FileNotFoundException ex) {
//        RunnerResponse runnerResponse =
//                new RunnerResponse(false, new ApiError(HttpStatus.NOT_FOUND, ex.getLocalizedMessage()));
        return ResponseHelper.buildFailedResponse(ex.getLocalizedMessage(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(RunnerException.class)
    public final ResponseEntity<IRunnerResponse> handleRunnerException(RunnerException ex) {
//        RunnerResponse runnerResponse =
//                new RunnerResponse(false, new ApiError(HttpStatus.INTERNAL_SERVER_ERROR, ex.getLocalizedMessage()));
        return ResponseHelper.buildFailedResponse(ex.getLocalizedMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(InvalidAuthenticationException.class)
    public final ResponseEntity<IRunnerResponse> handleAuthenticationException(RunnerException ex) {
//        RunnerResponse runnerResponse =
//                new RunnerResponse(false, new ApiError(HttpStatus.FORBIDDEN, ex.getLocalizedMessage()));
        return ResponseHelper.buildFailedResponse(ex.getLocalizedMessage(), HttpStatus.FORBIDDEN);
    }

    @Override
    protected ResponseEntity<Object> handleNoHandlerFoundException(
            NoHandlerFoundException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {

        return handleExceptionInternal(ex, "Path URL does not exist, Please check the URL", headers, status, request);
    }

    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(MethodArgumentNotValidException ex, HttpHeaders headers,
                                                                  HttpStatus status, WebRequest request) {
        List<String> errors = ex.getBindingResult()
                .getFieldErrors()
                .stream()
                .map(DefaultMessageSourceResolvable::getDefaultMessage)
                .toList();
        log.error("Return Response with data {}", errors.toString());
        ResponseEntity<IRunnerResponse> responseEntity = ResponseHelper.buildFailedResponse(Constants.Validation_Exception, errors);
        return ResponseEntity.status(responseEntity.getStatusCode()).body(responseEntity.getBody());
    }

    @Override
    public final ResponseEntity<Object> handleHttpMessageNotReadable(HttpMessageNotReadableException ex, HttpHeaders headers,
                                                                     HttpStatus status, WebRequest request) {
//        RunnerResponse runnerResponse =
//                new RunnerResponse(false, new ApiError(HttpStatus.BAD_REQUEST, ex.getLocalizedMessage()));
        ResponseEntity<IRunnerResponse> responseEntity = ResponseHelper.buildFailedResponse(ex.getLocalizedMessage(), HttpStatus.BAD_REQUEST);
        return ResponseEntity.status(responseEntity.getStatusCode()).body(responseEntity.getBody());
    }
}
