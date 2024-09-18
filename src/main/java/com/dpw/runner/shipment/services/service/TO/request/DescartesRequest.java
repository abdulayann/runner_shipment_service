package com.dpw.runner.shipment.services.service.TO.request;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DescartesRequest {

    private String apiVersion;
    private String contentType;
    private String contentCharset;
    private String contentEncoding;
    private String content;
}
