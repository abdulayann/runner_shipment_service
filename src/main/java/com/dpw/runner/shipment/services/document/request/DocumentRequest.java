package com.dpw.runner.shipment.services.document.request;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class DocumentRequest<T> implements Serializable {
    private T data;
    private DocumentOption options;
}