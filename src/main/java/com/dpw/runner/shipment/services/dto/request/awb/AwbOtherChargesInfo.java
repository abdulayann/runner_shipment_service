package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.LocationTypeCode;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import java.time.LocalDateTime;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Other Charges Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbOtherChargesInfo implements IRunnerResponse {
    private Long entityId;
    private String entityType;
    private String chargeTypeId;
    private BigDecimal rate;
    private Integer chargeBasis;
    private BigDecimal amount;
    private String modeOfPayment;
    private Integer chargeDue;
    @MasterData(type = MasterDataType.IATA_CHARGE_CODES)
    private String iataDescription;
    private String chargeTypeDescription;
    private BigDecimal awbChargeCodeDefaultVat;
    private Long v2ChargeId;
    private UUID guid;
    private UUID chargeTypeGuid;
    @Size(max = 5, message = "Additional ID must be up to 5 alphabetic characters.")
    @Pattern(regexp = "^[A-Za-z]*$", message = "Additional ID must contain only alphabetic characters.")
    private String additionalId;

    private LocationTypeCode locationTypeCode;

    @Size(max = 70, message = "Reason must be up to 70 alphanumeric characters.")
    private String reason;

    private String timeBasisQuantity;
    private String itemBasisQuantity;

    private LocalDateTime serviceDate;

    @Size(max = 35, message = "Special Service Description must be up to 35 characters.")
    private String specialServiceDescription;

    private LocalDateTime specialServiceTime;
}
