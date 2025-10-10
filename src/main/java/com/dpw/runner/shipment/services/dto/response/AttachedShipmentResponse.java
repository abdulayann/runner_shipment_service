package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Builder
@Schema(description = "Attached Shipment Response Model")
public class AttachedShipmentResponse implements IRunnerResponse {

    private Long attachedShipmentId;
    private String attachedShipmentNumber;
    private String attachedShipmentType;
}
