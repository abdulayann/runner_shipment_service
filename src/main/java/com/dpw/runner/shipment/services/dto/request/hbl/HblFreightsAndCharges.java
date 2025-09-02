package com.dpw.runner.shipment.services.dto.request.hbl;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ApiModel("Hbl Freights and Charges Data Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class HblFreightsAndCharges {

    private String charges;
    private Double value;
    private String currency;
    private String chargeType;
}
