package com.dpw.runner.shipment.services.document.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Map;

@AllArgsConstructor
@NoArgsConstructor
@Data @Builder
public class DocumentDownloadResponse implements  Serializable {

    byte[] content;
    Map headers;
}
