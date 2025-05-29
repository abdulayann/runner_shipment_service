package com.dpw.runner.shipment.services.document.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.http.HttpHeaders;

import java.io.Serializable;

@AllArgsConstructor
@NoArgsConstructor
@Data @Builder
public class DocumentDownloadResponse implements  Serializable {

    byte[] content;
    HttpHeaders headers;
}
