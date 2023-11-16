package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.responses.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.http.*;

import java.util.List;

/**
 * This helper is used to return generic response
 */
@SuppressWarnings("rawtypes")
@Slf4j
public class ResponseHelper {

    public static ResponseEntity<?> buildSuccessResponse(IRunnerResponse data, int pageNo, long count) {
        log.debug("Return Response with data {}", data);
        RunnerResponse runnerResponse = RunnerResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).count(count).pageNo(pageNo).build();
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }

    public static ResponseEntity<?> buildSuccessResponse(IRunnerResponse data) {
        log.debug("Return Response with data {}", data);
        return new ResponseEntity<>(RunnerResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).build(), HttpStatus.OK);
    }

    public static ResponseEntity<?> buildSuccessResponse(Object data) {
        log.debug("Return Response with data {}", data);
        return new ResponseEntity<>(RunnerResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).build(), HttpStatus.OK);
    }

    public static ResponseEntity<?> buildCreationSuccessResponse(IRunnerResponse data) {
        return new ResponseEntity<>(RunnerResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).build(), HttpStatus.CREATED);
    }

    public static ResponseEntity<?> buildSuccessResponse() {
        return new ResponseEntity<>(
                RunnerResponse.builder().success(true)
                        .requestId(LoggerHelper.getRequestIdFromMDC())
                        .build(),
                HttpStatus.OK);
    }

    public static ResponseEntity<?> buildListSuccessResponse(List<IRunnerResponse> data, int pageNo, long count) {
        log.debug("Return Response with data {}", data);
        RunnerListResponse runnerResponse = RunnerListResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).numberOfRecords(count).totalPages(pageNo).build();
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }


    public static ResponseEntity<?> buildListSuccessResponse(List<IRunnerResponse> data) {
        log.debug("Return Response with data {}", data);
        RunnerListResponse runnerResponse = RunnerListResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).build();
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }

    public static ResponseEntity<?> buildFailedResponse(String msg, HttpStatus httpStatus) {
        httpStatus = httpStatus == null  ? HttpStatus.BAD_REQUEST : httpStatus;
        log.debug("Return Response with error {}", msg);
        RunnerResponse runnerResponse = buildFailResponse(new ApiError(httpStatus, msg));
        return new ResponseEntity<>(runnerResponse, httpStatus);
    }

    public static ResponseEntity<?> buildFailedResponse(String msg) {
        log.debug("Return Response with error {}", msg);
        RunnerResponse runnerResponse = buildFailResponse(new ApiError(HttpStatus.BAD_REQUEST, msg));
        return new ResponseEntity<>(runnerResponse, HttpStatus.BAD_REQUEST);
    }

    public static ResponseEntity<?> buildFailedResponse(String msg, List<String> error) {
        log.debug("Return Response with error {}", msg);
        RunnerResponse runnerResponse = buildFailResponse(new ApiError(HttpStatus.BAD_REQUEST, msg, error));
        return new ResponseEntity<>(runnerResponse, HttpStatus.BAD_REQUEST);
    }

    public static ResponseEntity<?> buildResponse(HttpStatus httpStatus) {
        return new ResponseEntity<>(httpStatus);
    }

    public static RunnerResponse buildFailResponse(ApiError apiError) {
        log.debug("Return Response with error {}", apiError);
        return RunnerResponse.builder().success(false)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .error(apiError).build();
    }

    public static ResponseEntity<?> buildDependentServiceResponse(Object data, int pageNo, long count) {
        log.debug("Return Response with data {}", data);
        DependentServiceResponse runnerResponse = DependentServiceResponse.builder().success(true)
                .requestId(LoggerHelper.getRequestIdFromMDC())
                .data(data).numberOfRecords(count).pageSize(pageNo).build();
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }

    public static ResponseEntity<?> buildDependentServiceResponse(DependentServiceResponse runnerResponse) {
        log.debug("Return Response with data {}", runnerResponse);
        runnerResponse.setRequestId(LoggerHelper.getRequestIdFromMDC());
        return new ResponseEntity<>(runnerResponse, HttpStatus.OK);
    }

    public static ResponseEntity<?> buildFileResponse(byte[] bytes, MediaType contentType, String fileName) {
        ByteArrayResource resource = new ByteArrayResource(bytes);
        return ResponseEntity.ok()
                .contentType(contentType)
                .contentLength(resource.contentLength())
                .header(HttpHeaders.CONTENT_DISPOSITION,"attachment; filename=\"" + fileName + "\"")
                .body(resource);
    }
}

