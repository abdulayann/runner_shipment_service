package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.ArrivalNoticeModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Component
public class ArrivalNoticeReport extends IReport {

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        ArrivalNoticeModel arrivalNoticeModel = (ArrivalNoticeModel) getDocumentModel(id);
        return populateDictionary(arrivalNoticeModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        ArrivalNoticeModel arrivalNoticeModel = new ArrivalNoticeModel();
        arrivalNoticeModel.shipmentDetails = getShipment(id);
        if(arrivalNoticeModel.shipmentDetails != null && arrivalNoticeModel.shipmentDetails.getConsolidationList() != null && !arrivalNoticeModel.shipmentDetails.getConsolidationList().isEmpty())
        {
            arrivalNoticeModel.consolidationDetails = arrivalNoticeModel.shipmentDetails.getConsolidationList().get(0);
        }
        arrivalNoticeModel.hbl = getHbl(id);
        arrivalNoticeModel.containers = new ArrayList<>();
        if(arrivalNoticeModel.shipmentDetails.getContainersList() != null)
        {
            for(ContainerModel container : arrivalNoticeModel.shipmentDetails.getContainersList())
                arrivalNoticeModel.containers.add(getShipmentContainer(container));
        }
        arrivalNoticeModel.usersDto = UserContext.getUser();
        return arrivalNoticeModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ArrivalNoticeModel arrivalNoticeModel = (ArrivalNoticeModel) documentModel;
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(arrivalNoticeModel.shipmentDetails, GetDPWDateFormatOrDefault());
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(arrivalNoticeModel.shipmentDetails, false, dictionary);
        populateUserFields(arrivalNoticeModel.usersDto, dictionary);
        populateBlFields(arrivalNoticeModel.hbl, dictionary);
        populateShipmentOrganizationsLL(arrivalNoticeModel.shipmentDetails, dictionary);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(arrivalNoticeModel.containers));
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, arrivalNoticeModel.containers);
        if(StringUtility.isNotEmpty(arrivalNoticeModel.shipmentDetails.getHouseBill())) {
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_CARGOCONTROLNO, "80C2" + arrivalNoticeModel.shipmentDetails.getHouseBill());
        }
        getBillChargesDetails(arrivalNoticeModel);
        if(!Objects.isNull(arrivalNoticeModel.arrivalNoticeBillCharges) && !arrivalNoticeModel.arrivalNoticeBillCharges.isEmpty()){
            var currency = arrivalNoticeModel.arrivalNoticeBillCharges.stream().map(ArrivalNoticeModel.ArrivalNoticeBillCharges::getOverseasCurrency).filter(overseasCurrency ->!Objects.isNull(overseasCurrency)).findFirst().orElse("");
            BigDecimal sumOfTaxAmount = arrivalNoticeModel.arrivalNoticeBillCharges.stream()
                    .map(ArrivalNoticeModel.ArrivalNoticeBillCharges::getTaxAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            BigDecimal sumOfBillAmount = arrivalNoticeModel.arrivalNoticeBillCharges.stream()
                    .map(ArrivalNoticeModel.ArrivalNoticeBillCharges::getBillAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            dictionary.put(ReportConstants.SHIPMENT_BILLCHARGES_FREVENUEBILLCHARGES, arrivalNoticeModel.arrivalNoticeBillCharges);
            dictionary.put(ReportConstants.SHIPMENT_BILLCHARGES_BILLCHARGESLOCALTAXSUMCOMMA, AmountNumberFormatter.Format(sumOfTaxAmount, currency, TenantSettingsDetailsContext.getCurrentTenantSettings()));
            dictionary.put(ReportConstants.SHIPMENT_BILLCHARGES_BILLCHARGESSUM, AmountNumberFormatter.Format(sumOfBillAmount, currency, TenantSettingsDetailsContext.getCurrentTenantSettings()));
            dictionary.put(ReportConstants.SHIPMENT_BILLCHARGES_OVERSEASCURRENCY, currency);
        }

        return dictionary;
    }

    private void getBillChargesDetails(ArrivalNoticeModel arrivalNoticeModel){
        List<BillingResponse> billingsList = null;
        try {
            billingsList = getBillingData(arrivalNoticeModel.shipmentDetails.getGuid());
        }
        catch (Exception e) { }
        List<BillChargesResponse> charges = new ArrayList<>();
        BillingResponse billRow = null;
        if(billingsList != null && billingsList.size() > 0) {
            billRow = billingsList.get(0);
            for(BillingResponse billingResponse : billingsList) {
                List<BillChargesResponse> billChargesResponses = getBillChargesData(billingResponse.getGuid());
                if(billChargesResponses != null) {
                    for (BillChargesResponse charge : billChargesResponses) {
                        charges.add(charge);
                    }
                }
            }
        }

        arrivalNoticeModel.arrivalNoticeBillCharges = new ArrayList<>();
        if(!charges.isEmpty()) {
            for (var charge : charges){
                 var arrivalNoticeCharge = new ArrivalNoticeModel.ArrivalNoticeBillCharges();
                 arrivalNoticeCharge.setChargeTypeDescription(charge.getChargeTypeDescription());
                 arrivalNoticeCharge.setChargeTypeDescriptionLL(GetChargeTypeDescriptionLL(charge.getChargeTypeCode()));
                 if(!Objects.isNull(charge.getOverseasSellAmount())){
                     arrivalNoticeCharge.setSellAmount(AmountNumberFormatter.Format(charge.getOverseasSellAmount(), charge.getLocalSellCurrency(), TenantSettingsDetailsContext.getCurrentTenantSettings()));
                 }
                 if(!Objects.isNull(charge.getLocalTax())){
                     arrivalNoticeCharge.setTaxAmount(charge.getLocalTax());
                 }
                 if(!Objects.isNull(charge.getMeasurementBasis())){
                     arrivalNoticeCharge.setMeasurementBasis(MeasurementBasis.getByValue(Integer.parseInt(charge.getMeasurementBasis())).getDescription());
                 }
                 if(!Objects.isNull(charge.getOverseasSellAmount())){
                     arrivalNoticeCharge.setBillAmount(charge.getOverseasSellAmount());
                 }
                 if(!Objects.isNull(charge.getLocalCostCurrency())){
                     arrivalNoticeCharge.setOverseasCurrency(charge.getLocalCostCurrency());
                 }

                arrivalNoticeModel.arrivalNoticeBillCharges.add(arrivalNoticeCharge);
            }
        }

    }
}
