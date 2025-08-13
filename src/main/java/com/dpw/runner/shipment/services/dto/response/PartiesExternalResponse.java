package com.dpw.runner.shipment.services.dto.response;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;
import java.util.UUID;

@Data
@Builder
@ApiModel("Parties External Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class PartiesExternalResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String type;
    private String orgCode;
    private String addressCode;
    private String orgId;
    private String addressId;
    private Boolean isAddressFreeText;
    private String countryCode;
    private String message;
}