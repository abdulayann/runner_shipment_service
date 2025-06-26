package com.dpw.runner.shipment.services.exception.handler;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.*;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataRetrievalFailureException;
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
            BillingException.class,
            ReportException.class,
            RoutingException.class,
            NotificationException.class,
            GenericException.class,
            ValidationException.class,
            IllegalArgumentException.class,
            DataRetrievalFailureException.class,
            NotificationServiceException.class
    })
    private ResponseEntity<IRunnerResponse> handleCustomExceptions(final RuntimeException ex) {
        return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler({DpsException.class})
    private ResponseEntity<IRunnerResponse> handleDpsExceptions(final RuntimeException ex) {
        return ResponseHelper.buildFailedResponse("DPS ERROR -- "+ex.getMessage(), HttpStatus.BAD_REQUEST);
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
        return ResponseHelper.buildFailedResponse(ex.getLocalizedMessage(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(RunnerException.class)
    public final ResponseEntity<IRunnerResponse> handleRunnerException(RunnerException ex) {
        return ResponseHelper.buildFailedResponse(ex.getLocalizedMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(InvalidAuthenticationException.class)
    public final ResponseEntity<IRunnerResponse> handleAuthenticationException(InvalidAuthenticationException ex) {
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
                .map(x-> x.getField() + ": " + x.getDefaultMessage())
                .toList();
        String errorMessages = errors.size() > 1
                ? String.join(" | ", errors)
                : errors.get(0);
        log.error("Return Response with data {}", errors);
        ResponseEntity<IRunnerResponse> responseEntity = ResponseHelper.buildFailedResponse(errorMessages);
        return ResponseEntity.status(responseEntity.getStatusCode()).body(responseEntity.getBody());
    }

    @Override
    public final ResponseEntity<Object> handleHttpMessageNotReadable(HttpMessageNotReadableException ex, HttpHeaders headers,
                                                                     HttpStatus status, WebRequest request) {
        ResponseEntity<IRunnerResponse> responseEntity = ResponseHelper.buildFailedResponse(ex.getLocalizedMessage(), HttpStatus.BAD_REQUEST);
        return ResponseEntity.status(responseEntity.getStatusCode()).body(responseEntity.getBody());
    }
    @ExceptionHandler({SectionVisibilityException.class})
    private ResponseEntity<IRunnerResponse> handleSectionVisibilityException(final SectionVisibilityException ex) {
        return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.BAD_REQUEST);
    }
    @ExceptionHandler({SectionDetailsException.class})
    private ResponseEntity<IRunnerResponse> handleSectionDetailsException(final SectionDetailsException ex) {
        return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.BAD_REQUEST);
    }
    @ExceptionHandler({SectionFieldsException.class})
    private ResponseEntity<IRunnerResponse> handleSectionFieldsException(final SectionFieldsException ex) {
        return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.BAD_REQUEST);
    }
}
