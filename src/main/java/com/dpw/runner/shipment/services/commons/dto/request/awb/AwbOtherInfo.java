package com.dpw.runner.shipment.services.commons.dto.request.awb;

import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@Data
@Builder
@ApiModel("AWB Other Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbOtherInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private String shipper;
    private String carrier;
    @UnlocationData
    private String executedAt;
    private LocalDateTime executedOn;
}
