package com.dpw.runner.shipment.services.dto.response.OrderManagement;


import lombok.Data;

import java.io.Serializable;


@Data
public class ReferencesResponse implements Serializable {
    private String countryOfIssue;
    private String type;
    private String reference;
}
