package com.dpw.runner.shipment.services.dto.response.carrierbooking;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import lombok.Data;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;

@Data
public class VerifiedGrossMassBulkUpdateRequest {
    @Size(min = 2, message = "At least 2 containers must be selected for bulk update")
    private List<Long> containerIds;
    private WeightDeterminationMethodType weightDeterminationMethod;
    private String weightDeterminationLocation;
    private Parties weighingParty;
    private String approvalSignature;
    private LocalDateTime approvalDate;
}