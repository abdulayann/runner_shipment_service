package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ESIDetailsDTO {
    private String blReferenceNumber;
    private String status;
    private String version;
    private String bookingReferenceCode;

}
