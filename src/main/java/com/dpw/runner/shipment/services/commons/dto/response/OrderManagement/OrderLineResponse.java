package com.dpw.runner.shipment.services.commons.dto.response.OrderManagement;

import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

@Data
public class OrderLineResponse implements Serializable {
    private String itemId;
    private String itemDescription;
    private String skuId;
    private Integer innerPackCount;
    private String innerPackUnit;
    private String outerPackUnit;
    private Integer outerPackCount;
    private Integer quantity;
    private String quantityUnit;
    private Integer quantityInvoiced;
    private Integer quantityReceived;

    private BigDecimal itemPrice;
    private BigDecimal totalItemPrice;

    private String commercialInvoiceNumber;
    private String countryOfOrigin;
    private String manufacturerCode;
    private String manufacturerAddress;
    private String size;
    private String color;
}
