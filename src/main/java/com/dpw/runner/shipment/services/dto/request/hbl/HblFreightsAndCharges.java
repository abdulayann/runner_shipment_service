package com.dpw.runner.shipment.services.dto.request.hbl;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.io.Serializable;

@Data
@Schema("Hbl Freights and Charges Data Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class HblFreightsAndCharges implements Serializable {

    private String charges;
    private Double value;
    private String currency;
    private String chargeType;
}
