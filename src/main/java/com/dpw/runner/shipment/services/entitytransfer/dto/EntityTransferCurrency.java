package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.time.LocalDateTime;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferCurrency implements IEntityTranferBaseEntity {
    public Long fileId;
    public String CurrenyCode;
    public String CurrenyDescription;
    public Double ExchangeRate;
    public Double BuyRate;
    public LocalDateTime ActivateDate;
    public Double SellRate;
    public String BaseCurrency;
    public Boolean ReciprocalCurrency;
    public Double TransactionRate;
    public Double MarkUpExchangeRate;
    public LocalDateTime DeActivationDate;
}
