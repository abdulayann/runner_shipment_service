package com.dpw.runner.shipment.services.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@Data
@Builder
@Schema(description = "Customer Account Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class CustomerAccountInfo implements Serializable {

    private String accountHolder;
    private String accountName;
    private String accountIssuer;
    private String accountNumber;
    private String shippingFrequencyOrVolume;
    private String knownConsignorIndicator;
    private String billingType;
    private LocalDateTime establishmentDate;
    private String proofOfIdentification;
}
