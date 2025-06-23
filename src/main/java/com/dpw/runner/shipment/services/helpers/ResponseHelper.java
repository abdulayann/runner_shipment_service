package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.*;
import com.dpw.runner.shipment.services.dto.response.ByteArrayResourceResponse;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import java.util.List;

/**
 * This helper is used to return generic response
 */
@SuppressWarnings("rawtypes")
@Slf4j
public class ResponseHelper {

    private ResponseHelper(){}

    public static final String RETURN_RESPONSE_WITH_ERROR_MSG = "Return Response with error {}";
    public static final String RETURN_RESPONSE_WITH_DATA_MSG = "Return Response with data {}";

    public static ResponseEntity<IRunnerResponse> buildSuccessResponse(IRunnerResponse data, int pageNo, long count) {
        log.debug(RETURN_RESPONSE_WITH_DATA_MSG, data);
        RunnerResponse runnerResponse = RunnerResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).count(count).pageNo(pageNo).build();
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }

    public static ResponseEntity<IRunnerResponse> buildSuccessResponse(IRunnerResponse data) {
        log.debug(RETURN_RESPONSE_WITH_DATA_MSG, data);
        return new ResponseEntity<>(RunnerResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).build(), HttpStatus.OK);
    }

    public static ResponseEntity<IRunnerResponse> buildSuccessResponse(Object data) {
        log.debug(RETURN_RESPONSE_WITH_DATA_MSG, data);
        return new ResponseEntity<>(RunnerResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).build(), HttpStatus.OK);
    }

    public static ResponseEntity<IRunnerResponse> buildSuccessResponse() {
        return new ResponseEntity<>(
                RunnerResponse.builder().success(true)
                        .requestId(LoggerHelper.getRequestIdFromMDC())
                        .build(),
                HttpStatus.OK);
    }

    public static ResponseEntity<IRunnerResponse> buildSuccessResponseWithWarning(String warning) {
        return new ResponseEntity<>(
                RunnerResponse.builder().success(true)
                        .requestId(LoggerHelper.getRequestIdFromMDC())
                        .warning(warning)
                        .build(),
                HttpStatus.OK);
    }

    public static ResponseEntity<IRunnerResponse> buildListSuccessResponse(List<IRunnerResponse> data, int pageNo, long count) {
        IRunnerResponse runnerResponse = RunnerListResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).numberOfRecords(count).totalPages(pageNo).build();
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }

    public static ResponseEntity<IRunnerResponse> buildListSuccessResponse(List<IRunnerResponse> data) {
        log.debug(RETURN_RESPONSE_WITH_DATA_MSG, data);
        RunnerListResponse runnerResponse = RunnerListResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).build();
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }

    public static ResponseEntity<IRunnerResponse> buildFailedResponse(String msg, HttpStatus httpStatus) {
        httpStatus = httpStatus == null  ? HttpStatus.BAD_REQUEST : httpStatus;
        log.error(RETURN_RESPONSE_WITH_ERROR_MSG, msg);
        RunnerResponse runnerResponse = buildFailResponse(new ApiError(httpStatus, msg));
        return new ResponseEntity<>(runnerResponse, httpStatus);
    }

    public static ResponseEntity<IRunnerResponse> buildFailedResponse(String msg) {
        log.debug(RETURN_RESPONSE_WITH_ERROR_MSG, msg);
        RunnerResponse runnerResponse = buildFailResponse(new ApiError(HttpStatus.BAD_REQUEST, msg));
        return new ResponseEntity<>(runnerResponse, HttpStatus.BAD_REQUEST);
    }

    public static ResponseEntity<IRunnerResponse> buildFailedResponse(String msg, List<String> error) {
        log.debug(RETURN_RESPONSE_WITH_ERROR_MSG, msg);
        RunnerResponse runnerResponse = buildFailResponse(new ApiError(HttpStatus.BAD_REQUEST, msg, error));
        return new ResponseEntity<>(runnerResponse, HttpStatus.BAD_REQUEST);
    }

    public static RunnerResponse buildFailResponse(ApiError apiError) {
        log.debug(RETURN_RESPONSE_WITH_ERROR_MSG, apiError);
        return RunnerResponse.builder().success(false)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .error(apiError).build();
    }

    public static ResponseEntity<IRunnerResponse> buildDependentServiceResponse(Object data, int pageNo, long count) {
        log.debug(RETURN_RESPONSE_WITH_DATA_MSG, data);
        DependentServiceResponse runnerResponse = DependentServiceResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).numberOfRecords(count).pageSize(pageNo).build();
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }

    public static ResponseEntity<IRunnerResponse> buildDependentServiceResponse(DependentServiceResponse runnerResponse) {
        log.debug(RETURN_RESPONSE_WITH_DATA_MSG, runnerResponse);
        runnerResponse.setRequestId(LoggerHelper.getRequestIdFromMDC());
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }


    public static ResponseEntity<IRunnerResponse> buildFileResponse(byte[] bytes, MediaType contentType, String fileName) {
        ByteArrayResourceResponse resource = new ByteArrayResourceResponse(bytes);
        return ResponseEntity.ok()
                .contentType(contentType)
                .contentLength(resource.contentLength())
                .header(HttpHeaders.CONTENT_DISPOSITION,"attachment; filename=" + fileName)
                .header("X-CSD-Document-Status",
                    String.valueOf(Boolean.parseBoolean(MDC.get(Constants.IS_CSD_DOCUMENT_ADDED))))
                .body(resource);
    }
}

