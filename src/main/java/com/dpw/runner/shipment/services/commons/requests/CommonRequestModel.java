package com.dpw.runner.shipment.services.commons.requests;

import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Data
@Builder
@Slf4j
public class CommonRequestModel {
    public static final String RECEIVED_REQUEST_MSG = "Received Request {}";
    private Long id;
    private String guid;
    private IRunnerRequest data;
    private List<? extends IRunnerRequest> dataList;
    @Builder.Default
    private Integer pageNo = 0;
    @Builder.Default
    private Integer count = 50;
    @Builder.Default
    private Boolean active = true;
    private Object dependentData;

    public static CommonRequestModel buildRequest(IRunnerRequest data, int pageNo, int count) {
        log.debug(RECEIVED_REQUEST_MSG, data);
        return CommonRequestModel.builder().data(data).count(count).pageNo(pageNo).build();
    }

    public static CommonRequestModel buildRequest(IRunnerRequest data) {
        log.debug(RECEIVED_REQUEST_MSG, data);
        return CommonRequestModel.builder().data(data).build();
    }

    public static CommonRequestModel buildRequest(String guid, IRunnerRequest data) {
        log.debug(RECEIVED_REQUEST_MSG, data);
        return CommonRequestModel.builder().guid(guid).data(data).build();
    }

    public static CommonRequestModel buildRequest(int pageNo, int count, Boolean isActive) {
        return CommonRequestModel.builder().count(count).pageNo(pageNo).active(isActive == null || isActive).build();
    }

    public static CommonRequestModel buildRequest(String guid, int pageNo, int count) {
        return CommonRequestModel.builder().guid(guid).count(count).pageNo(pageNo).build();
    }

    public static CommonRequestModel buildRequest(String guid) {
        return CommonRequestModel.builder().guid(guid).build();
    }

    public static CommonRequestModel buildRequest() {
        return CommonRequestModel.builder().build();
    }

    public static CommonRequestModel buildRequest(Long id) {
        return CommonRequestModel.builder().id(id).build();
    }

    public static CommonRequestModel buildRequest(Boolean isActive) {
        return CommonRequestModel.builder().active(isActive == null || isActive).build();
    }

    public static CommonRequestModel buildRequest(IRunnerRequest data, Boolean isActive) {
        log.debug(RECEIVED_REQUEST_MSG, data);
        return CommonRequestModel.builder().data(data).active(isActive == null || isActive).build();
    }

    public static CommonRequestModel buildRequest(List<? extends IRunnerRequest> dataList) {
        return CommonRequestModel.builder().dataList(dataList).build();
    }

    public static CommonRequestModel buildDependentDataRequest(Object data) {
        log.debug(RECEIVED_REQUEST_MSG, data);
        return CommonRequestModel.builder().dependentData(data).build();
    }

}
