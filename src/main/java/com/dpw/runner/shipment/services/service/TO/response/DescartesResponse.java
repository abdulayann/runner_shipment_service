package com.dpw.runner.shipment.services.service.TO.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DescartesResponse {

    private String host;
    private String service;
    private String version;
    private String status;
    private String apiVersion;
    private String bytesReceived;
    private String tid;
    private String error;
    private String errorShort;
    private String errorDetail;
    private String unhandledException;
    private Integer retryAfter;
    @JsonProperty("ProcessingLog")
    private Object processingLog;
}