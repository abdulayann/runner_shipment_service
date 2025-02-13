package com.dpw.runner.shipment.services.document.request;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
@Generated
public class DocumentRequest<T> implements Serializable {
    private T data;
    private DocumentOption options;
}