package com.dpw.runner.shipment.services.commons.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ESIDetailsDTO implements Serializable {
    private String blReferenceNumber;
    private String status;
    private String version;
    private String bookingReferenceCode;

}
