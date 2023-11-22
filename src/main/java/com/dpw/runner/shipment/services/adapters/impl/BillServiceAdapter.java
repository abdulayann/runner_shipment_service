package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IBillServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

@Service
@Slf4j
public class BillServiceAdapter implements IBillServiceAdapter {
    private final RestTemplate restTemplate;
    private final String baseUrl;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    JsonHelper jsonHelper;


    public BillServiceAdapter(RestTemplate restTemplate, @Value("${external.bill.update.or.create}") String baseUrl) {
        this.restTemplate = restTemplate;
        this.baseUrl = baseUrl;
    }


    public ResponseEntity<?> createBillcharges(CommonRequestModel requestModel, String shipment_guid, String clientOrgCode, String clientAddressCode) throws Exception {
        BookingChargesRequest request = (BookingChargesRequest) requestModel.getData();
        ExternalBillPayload externalRequest= new ExternalBillPayload();
        List<ExternalBillRequest> externalBillRequestList =new ArrayList<>();

        ExternalBillRequest externalBillRequest=new ExternalBillRequest();

        CommonV1ListRequest orgRequest = new CommonV1ListRequest();
        List<Object> orgCriteria = new ArrayList<>();
        List<Object> orgField = new ArrayList<>(List.of("OrganizationCode"));
        String op = "=";
        orgCriteria.addAll(List.of(orgField, op, clientOrgCode));
        orgRequest.setCriteriaRequests(orgCriteria);
        V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
        List<EntityTransferOrganizations> orgList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
        long orgId=orgList.get(0).getId();

        List<Object> finalCriteria= new ArrayList<>();
        CommonV1ListRequest addressReq = new CommonV1ListRequest();
        List<Object>addressCriteria =new ArrayList<>();
        List<Object> addressField = new ArrayList<>(List.of("AddressShortCode"));
        addressCriteria.addAll(List.of(addressField, op, clientAddressCode));
        finalCriteria.add(addressCriteria);

        finalCriteria.add("and");

        List<Object>orgIdCriteria=new ArrayList<>();
        List<Object> orgIdfield = new ArrayList<>(List.of("OrgId"));
        orgIdCriteria.add(List.of(orgIdfield, op, orgId));
        finalCriteria.addAll(orgIdCriteria);

        addressReq.setCriteriaRequests(finalCriteria);
        V1DataResponse addressResponse = v1Service.addressList(addressReq);
        List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);
        long addressId=addressList.get(0).getId();
        BillRequest billRequest= new BillRequest();
        billRequest.setModuleType("SHIPMENT");
        billRequest.setModuleGuid(shipment_guid);
        billRequest.setModuleRef(shipment_guid);
        billRequest.setClientId(orgId);
        billRequest.setClientAddressId(addressId);
        externalBillRequest.setExternalBill(billRequest);



        List<ExternalBillChargeRequest> externalBillCharges= new ArrayList<>();
        ExternalBillChargeRequest externalBillChargeRequest=new ExternalBillChargeRequest();
        BillChargesRequest billChargesRequest=new BillChargesRequest();
        BillChargeRevenueDetailsRequest billChargeRevenueDetailsRequest=new BillChargeRevenueDetailsRequest();
        BillChargeCostDetailsRequest billChargeCostDetailsRequest = new BillChargeCostDetailsRequest();

        MeasurementBasis measurementBasis=MeasurementBasis.Container_Count;

        if(request.getMeasurementBasis()=="Container_Count"){
         measurementBasis=MeasurementBasis.Container_Count;
        }
        if(request.getMeasurementBasis()=="Weight"){
            measurementBasis=MeasurementBasis.Weight;
        }
        if(request.getMeasurementBasis()=="Volume"){
            measurementBasis=MeasurementBasis.Volume;
        }
        if(request.getMeasurementBasis()=="Chargeable"){
            measurementBasis=MeasurementBasis.Chargeable;
        }
        if(request.getMeasurementBasis()=="Lowest_Bill"){
            measurementBasis=MeasurementBasis.Lowest_Bill;
        }
        if(request.getMeasurementBasis()=="Package"){
            measurementBasis=MeasurementBasis.Package;
        }
        if(request.getMeasurementBasis()=="Shipment"){
            measurementBasis=MeasurementBasis.Shipment;
        }
        if(request.getMeasurementBasis()=="TEU"){
            measurementBasis=MeasurementBasis.TEU;
        }
        if(request.getMeasurementBasis()=="Charge_Percentage"){
            measurementBasis=MeasurementBasis.Charge_Percentage;
        }
        if(request.getMeasurementBasis()=="Custom"){
            measurementBasis=MeasurementBasis.Custom;
        }
        if(request.getMeasurementBasis()=="Container_Type"){
            measurementBasis=MeasurementBasis.Container_Type;
        }

        ExternalBillConfiguration externalBillConfiguration = new ExternalBillConfiguration();
        List<String>autoCalculate = new ArrayList<>();
        autoCalculate.add("Tax");
        autoCalculate.add("SequenceNumber");
        autoCalculate.add("LocalReferenceNumber");
        autoCalculate.add("RevenueVendorSection");
        externalBillConfiguration.setAutoCalculate(autoCalculate);

        billChargeCostDetailsRequest.setMeasurementBasis(measurementBasis);
        billChargeCostDetailsRequest.setMeasurementBasisQuantity(request.getTotalUnitCount());
        billChargeCostDetailsRequest.setMeasurementBasisUnit("SHIPMENT");
        billChargeCostDetailsRequest.setLocalCostAmount(request.getLocalCostAmount());
        billChargeCostDetailsRequest.setOverseasCostAmount(request.getOverseasCostAmount());
        billChargeCostDetailsRequest.setOverseasCostCurrency(request.getOverseasCostCurrency());
        billChargeCostDetailsRequest.setLocalCostCurrency(request.getLocalCostCurrency());
        billChargeCostDetailsRequest.setCreditorId(request.getCreditor().getId().toString());
        billChargeCostDetailsRequest.setCreditorAddressId(request.getCreditor().getAddressCode());


        billChargeRevenueDetailsRequest.setMeasurementBasis(measurementBasis);
        billChargeRevenueDetailsRequest.setLocalSellAmount(request.getLocalSellAmount());
        billChargeRevenueDetailsRequest.setOverseasSellAmount(request.getOverseasSellAmount());
        billChargeRevenueDetailsRequest.setOverseasSellCurrency(request.getOverseasSellCurrency());
        billChargeRevenueDetailsRequest.setLocalSellCurrency(request.getLocalSellCurrency());
        billChargeRevenueDetailsRequest.setTaxPercentage(request.getTaxPercentage());
        billChargeRevenueDetailsRequest.setOverseasTax(request.getOverseasTax());
        billChargeRevenueDetailsRequest.setMeasurementBasisQuantity(request.getTotalUnitCount());

        billChargesRequest.setBillChargeRevenueDetails(billChargeRevenueDetailsRequest);
        billChargesRequest.setBillChargeCostDetails(billChargeCostDetailsRequest);
        billChargesRequest.setChargeTypeId(request.getChargeType());

        externalBillChargeRequest.setBillChargeRequest(billChargesRequest);
        externalBillCharges.add(externalBillChargeRequest);
        externalBillRequest.setExternalBillCharges(externalBillCharges);
        externalBillRequest.setConfiguration(externalBillConfiguration);
        externalBillRequestList.add(externalBillRequest);
        externalRequest.setExternalBillRequestList(externalBillRequestList);





        String url = baseUrl;
        try {
            ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)), Object.class);
            return ResponseHelper.buildDependentServiceResponse(response.getBody(), 0, 0);
        } catch (Exception ex) {
            log.error("Bill charge create failed due to: {}", jsonHelper.convertToJson(ex.getMessage()));
            throw new RuntimeException("Error while creating the bill charge: " + ex.getMessage());
        }
    }
}
