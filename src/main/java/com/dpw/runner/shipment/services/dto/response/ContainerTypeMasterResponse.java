package com.dpw.runner.shipment.services.dto.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.Setter;

import java.math.BigDecimal;

@Data
@Setter
@ApiModel("Container Type Master Response Model")
public class ContainerTypeMasterResponse {

    @JsonProperty("Id")
    private Long id;

    @JsonProperty("Code")
    private String code;

    @JsonProperty("Description")
    private String description;

    @JsonProperty("Mode")
    private String mode;

    @JsonProperty("Teu")
    private BigDecimal teu;

    @JsonProperty("Active")
    private Boolean active;

    @JsonProperty("IsTank")
    private Boolean isTank;

    @JsonProperty("IsReeferTempControlled")
    private Boolean isReeferTempControlled;

    @JsonProperty("TextField")
    private String textField;

    @JsonProperty("IsActive")
    private String isActive;

    @JsonProperty("TareWeight")
    private BigDecimal tareWeight;

    @JsonProperty("MaxCargoGrossWeight")
    private Double maxCargoGrossWeight;

    @JsonProperty("MaxCargoGrossWeightUnit")
    private String maxCargoGrossWeightUnit;

    @JsonProperty("CubicCapacity")
    private Double cubicCapacity;

    @JsonProperty("CubicCapacityUnit")
    private String cubicCapacityUnit;

    @JsonProperty("UpdateDate")
    private String updateDate;

    @JsonProperty("IsoCode")
    private String isoCode;

    @JsonProperty("IntegrationCode")
    private String integrationCode;

    @JsonProperty("IsQuoted")
    private Boolean isQuoted;
}
