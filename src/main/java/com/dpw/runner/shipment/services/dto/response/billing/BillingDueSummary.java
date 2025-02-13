package com.dpw.runner.shipment.services.dto.response.billing;

import lombok.*;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class BillingDueSummary {
    String branchId;
    String moduleGuid;
    Boolean dueRemaining;
}