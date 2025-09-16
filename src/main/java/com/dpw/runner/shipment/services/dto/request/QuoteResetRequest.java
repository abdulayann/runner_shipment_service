package com.dpw.runner.shipment.services.dto.request;

import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class QuoteResetRequest {
    private Long shipmentId;
    private Long bookingId;
    private Boolean quotePartyResetFlag;
    private Boolean transportModeResetFlag;
    private Boolean cargoTypeResetFlag;
    private Boolean serviceTypeResetFlag;
    private Boolean originResetFlag;
    private Boolean polResetFlag;
    private Boolean podResetFlag;
    private Boolean destinationResetFlag;
    private Boolean salesBranchResetFlag;
    private Boolean primaryEmailResetFlag;
    private Boolean secondaryEmailResetFlag;
}
