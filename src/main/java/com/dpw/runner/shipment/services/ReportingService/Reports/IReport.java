package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ContainerCountByCode;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public abstract class IReport {

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IHblDao hblDao;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAwbRepository awbRepository;

    public abstract Map<String, Object> getData(Long id);
    abstract IDocumentModel getDocumentModel(Long id);
    abstract Map<String, Object> populateDictionary(IDocumentModel documentModel);

    public ShipmentContainers getShipmentContainer(Containers row)
    {
        ShipmentContainers ship = new ShipmentContainers();
        ship.ContainerNumber = row.getContainerNumber();
        ship.SealNumber = row.getSealNumber();
        ship.NoofPackages = row.getNoOfPackages();
        ship.ShipmentPacks = Long.valueOf(row.getPacks());
        ship.ShipmentPacksUnit = row.getPacksType();
        ship.GrossWeight = row.getGrossWeight().setScale(2, RoundingMode.HALF_UP);
        ship.GrossWeightUnit = row.getGrossWeightUnit();
        ship.TareWeight = row.getTareWeight().setScale(2, RoundingMode.HALF_UP);
        ship.TareWeightUnit = row.getTareWeightUnit();
        ship.Measurement = row.getMeasurement().setScale(2, RoundingMode.HALF_UP);
        ship.MeasurementUnit = row.getMeasurementUnit();
        ship.GrossVolume = row.getGrossVolume().setScale(2, RoundingMode.HALF_UP);
        ship.GrossVolumeUnit = row.getGrossVolumeUnit();
        ship.ContainerTypeCode = row.getContainerCode();
        ship.ContainerCount = row.getContainerCount();
        ship.ShipmentMarksnNums = row.getMarksNums();
        ship.NetWeight = row.getNetWeight().setScale(2, RoundingMode.HALF_UP);
        ship.NetWeightUnit = row.getNetWeightUnit();
        ship.MinTemp = row.getMinTemp().setScale(2, RoundingMode.HALF_UP);
        ship.MinTempUnit = row.getMinTempUnit();
        ship.ShipmentHblDeliveryMode = row.getHblDeliveryMode();
        ship.DescriptionOfGoods = row.getDescriptionOfGoods();
        ship.CarrierSealNumber = row.getCarrierSealNumber();
        ship.CustomsSealNumber = row.getCustomsSealNumber();
        ship.ShipperSealNumber = row.getShipperSealNumber();
        return ship;
    }

    public void populateBLContainer(ShipmentContainers shipmentContainer, HblContainerDto blObjectContainer)
    {
        shipmentContainer.BL_ContainerType = blObjectContainer.getContainerType();
        shipmentContainer.BL_SealNumber = blObjectContainer.getSealNumber();
        shipmentContainer.BL_GrossWeight = blObjectContainer.getContainerGrossWeight().setScale(2, RoundingMode.HALF_UP);
        shipmentContainer.BL_GrossWeightUnit = blObjectContainer.getContainerGrossWeightUnit();
        shipmentContainer.BL_GrossVolume = blObjectContainer.getContainerGrossVolume().setScale(2, RoundingMode.HALF_UP);
        shipmentContainer.BL_GrossVolumeUnit = blObjectContainer.getContainerGrossVolumeUnit();
        shipmentContainer.BL_NoofPackages = blObjectContainer.getNoOfPackages();
        shipmentContainer.BL_CarrierSealNumber = blObjectContainer.getCarrierSealNumber();
        shipmentContainer.BL_ContainerNumber = blObjectContainer.getContainerNumber();
        shipmentContainer.BL_ContainerDescription = blObjectContainer.getContainerDesc();
        shipmentContainer.BL_PackageUnit = blObjectContainer.getPackageUnit();
    }

    public void populateShipmentFields(ShipmentDetails shipment, Boolean isHBL, Map<String, Object> dictionary)
    {
        if (shipment == null) {
            return;
        }

        PickupDeliveryDetails pickup = shipment.getPickupDetails();
        PickupDeliveryDetails delivery = shipment.getDeliveryDetails();

        Parties shipmentClient = shipment.getClient();
        Parties shipmentConsignee = shipment.getConsignee();
        Parties shipmentConsigner = shipment.getConsigner();
        Parties shipmentNotify = shipment.getAdditionalDetails().getNotifyParty();

        UnlocationsResponse pol = null, pod = null, origin = null, destination = null, paidPlace = null, placeOfIssue = null, placeOfSupply = null;

        List<Object> criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getCarrierDetails().getOriginPort()
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            pol = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getCarrierDetails().getDestinationPort()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            pod = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getCarrierDetails().getOrigin()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            origin = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getCarrierDetails().getDestination()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            destination = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getAdditionalDetails().getPaidPlace()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            paidPlace = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getAdditionalDetails().getPlaceOfIssue()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            placeOfIssue = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getAdditionalDetails().getPlaceOfSupply()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            placeOfSupply = unlocationsResponse.get(0);

        dictionary.put(ReportConstants.MASTER_BILL,shipment.getMasterBill());
        dictionary.put(ReportConstants.HOUSE_BILL,shipment.getHouseBill());
        dictionary.put(ReportConstants.VESSEL_NAME, shipment.getCarrierDetails().getVessel());
        dictionary.put(ReportConstants.VOYAGE,shipment.getCarrierDetails().getVoyage());
        dictionary.put(ReportConstants.POL_CODE, shipment.getCarrierDetails().getOriginPort());
        dictionary.put(ReportConstants.POD_CODE, shipment.getCarrierDetails().getDestinationPort());
        dictionary.put(ReportConstants.POD_COUNTRY, pod != null ? pod.getCountry() : null);
        dictionary.put(ReportConstants.POL_COUNTRY, pol != null ? pol.getCountry() : null);
        dictionary.put(ReportConstants.POL_PORTNAME, pol != null ? pol.getPortName() : null);
        dictionary.put(ReportConstants.POD_PORTNAME, pod != null ? pod.getPortName() : null);
        dictionary.put(ReportConstants.CARRIER,shipment.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.PORT_OF_DISCHARGE, pod != null ? pod.getName() : null);
        dictionary.put(ReportConstants.PORT_OF_LOADING, pol != null ? pol.getName() : null);
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.REFERENCE_NUMBER,shipment.getBookingReference());
        dictionary.put(ReportConstants.ORIGIN_NAME, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.ORIGIN_COUNTRY, origin != null ? origin.getCountry() : null);
        dictionary.put(ReportConstants.DESCRIPTION,shipment.getGoodsDescription());
        dictionary.put(ReportConstants.SHIPMENT_TYPE,shipment.getDirection());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, shipment.getDirection() != null ? Character.toUpperCase(shipment.getDirection().charAt(0)) : null);
        Long containerCount = 0L;
        if(shipment.getContainersList().size() > 0)
        {
            for (Containers container : shipment.getContainersList())
            {
                if (container.getContainerCount() != 0)
                {
                    containerCount += container.getContainerCount();
                }
            }
        }
        dictionary.put(ReportConstants.CONTAINER_COUNT, containerCount);

        dictionary.put(ReportConstants.ETA, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getEta() : null);
        dictionary.put(ReportConstants.ETD,shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getEtd() : null);
        dictionary.put(ReportConstants.ATA,shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getAta() : null);
        dictionary.put(ReportConstants.ATD,shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getAtd() : null);
        dictionary.put(ReportConstants.DATE_OF_DEPARTURE, dictionary.get(ReportConstants.ATD) == null ? dictionary.get(ReportConstants.ETD) : dictionary.get(ReportConstants.ATD));
        dictionary.put(ReportConstants.SYSTEM_DATE, LocalDateTime.now());
        dictionary.put(ReportConstants.ONBOARD_DATE, shipment.getAdditionalDetails().getOnBoardDate());
        dictionary.put(ReportConstants.ESTIMATED_READY_FOR_PICKUP, pickup != null ? pickup.getEstimatedPickupOrDelivery() : null);
        dictionary.put(ReportConstants.DATE_OF_ISSUE, shipment.getAdditionalDetails().getDateOfIssue());
        dictionary.put(ReportConstants.DATE_OF_RECEIPT, shipment.getAdditionalDetails().getDateOfReceipt());

        dictionary.put(ReportConstants.INCO_TERM, shipment.getIncoterms());
        dictionary.put(ReportConstants.CHARGEABLE, shipment.getChargable());
        dictionary.put(ReportConstants.CHARGEABLE_UNIT, shipment.getChargeableUnit());
        dictionary.put(ReportConstants.TRANSPORT_MODE, shipment.getTransportMode());
        MasterData masterData = getMasterListData(MasterDataType.TRANSPORT_MODE, shipment.getTransportMode());
        dictionary.put(ReportConstants.TRANSPORT_MODE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getTransportMode());
        masterData = getMasterListData(MasterDataType.CUSTOM_SHIPMENT_TYPE, shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_TYPE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_NUMBER, shipment.getShipmentId());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, shipment.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.ADDITIONAL_TERMS, shipment.getAdditionalTerms());

        dictionary.put(ReportConstants.PACKS,shipment.getNoOfPacks());
        dictionary.put(ReportConstants.PACKS_UNIT,shipment.getPacksUnit());
        masterData = getMasterListData(MasterDataType.PACKS_UNIT, shipment.getPacksUnit());
        dictionary.put(ReportConstants.PACKS_UNIT_DESC, masterData != null && !masterData.getItemDescription().isEmpty() ? masterData.getItemDescription() : shipment.getPacksUnit());
        dictionary.put(ReportConstants.GROSS_WEIGHT,shipment.getWeight());
        dictionary.put(ReportConstants.GROSS_WEIGHT_UNIT,shipment.getWeightUnit());
        dictionary.put(ReportConstants.GROSS_VOLUME,shipment.getVolume());
        dictionary.put(ReportConstants.GROSS_VOLUME_UNIT,shipment.getVolumeUnit());

        dictionary.put(ReportConstants.DELIVERY_CFS, delivery != null && delivery.getSourceDetail() != null ? delivery.getSourceDetail().getOrgData().get("FullName") : null);
        dictionary.put(ReportConstants.PICKUP_CFS, pickup != null && pickup.getDestinationDetail() != null ? pickup.getDestinationDetail().getOrgData().get("FullName") : null);
        dictionary.put(ReportConstants.MARKS_N_NUMS,shipment.getMarksNum());
        dictionary.put(ReportConstants.ORIGINALS,shipment.getAdditionalDetails().getOriginal() == null ? 1 : shipment.getAdditionalDetails().getOriginal());
        dictionary.put(ReportConstants.ORIGINAL_WORDS, "");
        dictionary.put(ReportConstants.COPY_BILLS,shipment.getAdditionalDetails().getCopy() == null ? 0 : shipment.getAdditionalDetails().getCopy());

        dictionary.put(ReportConstants.ISSUE_PLACE_NAME, placeOfIssue !=  null ? placeOfIssue.getName() : null);
        dictionary.put(ReportConstants.ISSUE_PLACE_COUNTRY, placeOfIssue != null ? placeOfIssue.getCountry() : null);
        dictionary.put(ReportConstants.PAID_PLACE_NAME, paidPlace != null ? paidPlace.getName() : null);
        dictionary.put(ReportConstants.PAID_PLACE_COUNTRY, paidPlace != null ? paidPlace.getCountry() : null);

        dictionary.put(ReportConstants.HSN_NUMBER, shipment.getAdditionalDetails().getHsnNumber());
        dictionary.put(ReportConstants.SHIPMENT_BOOKING_NUMBER, shipment.getBookingNumber());

        dictionary.put(ReportConstants.DESTINATION_NAME_, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.DESTINATION_COUNTRY, destination != null ? destination.getCountry() : null);

        if (Objects.equals(shipment.getPaymentTerms(), "PPD")) {
            dictionary.put(ReportConstants.AT, pol != null ? pol.getName() : null);
        } else if (Objects.equals(shipment.getPaymentTerms(), "CCX")) {
            dictionary.put(ReportConstants.AT, pod != null ? pod.getName() : null);
        }

        var array = new String[] {"" + dictionary.get("VesselName"), shipment.getCarrierDetails().getVoyage()};
        dictionary.put(ReportConstants.VESSEL_NAME_AND_VOYAGE, array[0] + " & " + array[1]);

        dictionary.put(ReportConstants.WAREHOUSE_NAME, shipment.getAdditionalDetails().getWarehouseId());
        masterData = null;
        if(placeOfIssue != null)
        {
            masterData = getMasterListData(MasterDataType.COUNTRIES, placeOfIssue.getCountry());
        }
        dictionary.put(ReportConstants.ISSUEPLACECOUNTRYNAME, masterData != null ? masterData.getItemDescription() : null);

        if (!isHBL && (Objects.equals(shipment.getTransportMode(), "SEA") || Objects.equals(shipment.getTransportMode(), "ROA") || Objects.equals(shipment.getTransportMode(), "RF"))) {
            List<String> consigner = null;
            if(shipmentConsigner != null)
            {
                Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
                if(consignerAddress != null)
                {
                    consigner = ReportHelper.getOrgAddressWithPhoneEmail(consignerAddress.get("CompanyName").toString(), consignerAddress.get("Address1").toString(),
                                                                         consignerAddress.get("Address2").toString(),
                                                                         ReportHelper.getCityCountry(consignerAddress.get("City").toString(), consignerAddress.get("Country").toString()),
                                                                         consignerAddress.get("Email").toString(), consignerAddress.get("ContactPhone").toString(),
                                                                         consignerAddress.get("Zip_PostCode").toString()
                                                                        );
                    dictionary.put(ReportConstants.CONSIGNER_NAME, consignerAddress.get("CompanyName"));
                    dictionary.put(ReportConstants.CONSIGNER_CONTACT_PERSON, consignerAddress.get("ContactPerson"));
                }
                if(shipmentConsigner.getOrgData() != null)
                    dictionary.put(ReportConstants.CONSIGNER_LOCAL_NAME,shipmentConsigner.getOrgData().get("LocalName"));
            }
            List<String> consignee = null;
            if(shipmentConsignee != null)
            {
                Map<String, Object> consigneeAddress = shipmentConsignee.getAddressData();
                if(consigneeAddress != null)
                {
                    consignee = ReportHelper.getOrgAddressWithPhoneEmail(consigneeAddress.get("CompanyName").toString(), consigneeAddress.get("Address1").toString(),
                                                                         consigneeAddress.get("Address2").toString(),
                                                                         ReportHelper.getCityCountry(consigneeAddress.get("City").toString(), consigneeAddress.get("Country").toString()),
                                                                         consigneeAddress.get("Email").toString(), consigneeAddress.get("ContactPhone").toString(),
                                                                         consigneeAddress.get("Zip_PostCode").toString()
                                                                        );
                    dictionary.put(ReportConstants.CONSIGNEE_NAME,consigneeAddress.get("CompanyName"));
                    dictionary.put(ReportConstants.CONSIGNEE_CONTACT_PERSON,consigneeAddress.get("ContactPerson"));
                }
                if(shipmentConsignee.getOrgData() != null)
                    dictionary.put(ReportConstants.CONSIGNEE_LOCAL_NAME, shipmentConsignee.getOrgData().get("LocalName"));
            }
            List<String> notify = null;
            if(shipmentNotify != null)
            {
                Map<String, Object> notifyAddress = shipmentNotify.getAddressData();
                if(notifyAddress != null)
                {
                    notify = ReportHelper.getOrgAddressWithPhoneEmail(notifyAddress.get("CompanyName").toString(), notifyAddress.get("Address1").toString(),
                                                                      notifyAddress.get("Address2").toString(),
                                                                      ReportHelper.getCityCountry(notifyAddress.get("City").toString(), notifyAddress.get("Country").toString()),
                                                                      notifyAddress.get("Email").toString(), notifyAddress.get("ContactPhone").toString(),
                                                                      notifyAddress.get("Zip_PostCode").toString()
                                                                     );
                    dictionary.put(ReportConstants.NOTIFY_PARTY_NAME,notifyAddress.get("CompanyName"));
                    dictionary.put(ReportConstants.NOTIFY_PARTY_CONTACT_PERSON,notifyAddress.get("ContactPerson"));
                }
                if(shipmentNotify.getOrgData() != null)
                    dictionary.put(ReportConstants.NOTIFY_PARTY_LOCAL_NAME,shipmentNotify.getOrgData().get("LocalName"));
            }
            List<String> client = null;
            if(shipmentClient != null)
            {
                Map<String, Object> clientAddress = shipmentClient.getAddressData();
                if(clientAddress != null)
                {
                    client = ReportHelper.getOrgAddressWithPhoneEmail(clientAddress.get("CompanyName").toString(), clientAddress.get("Address1").toString(),
                                                                      clientAddress.get("Address2").toString(),
                                                                      ReportHelper.getCityCountry(clientAddress.get("City").toString(), clientAddress.get("Country").toString()),
                                                                      clientAddress.get("Email").toString(), clientAddress.get("ContactPhone").toString(),
                                                                      clientAddress.get("Zip_PostCode").toString()
                                                                     );
                    dictionary.put(ReportConstants.CLIENT_NAME, clientAddress.get("CompanyName"));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_1,clientAddress.get("Address1"));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_PHONE,clientAddress.get("ContactPhone"));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_MOBILE,clientAddress.get("Mobile"));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_CONTACT_PERSON, clientAddress.get("ContactPerson"));
                }
            }

            dictionary.put(ReportConstants.CONSIGNER,consigner);
            dictionary.put(ReportConstants.CONSIGNEE,consignee);
            dictionary.put(ReportConstants.NOTIFY_PARTY, notify);
            dictionary.put(ReportConstants.CONSIGNEE_FREETEXT,dictionary.get(ReportConstants.CONSIGNEE));
            dictionary.put(ReportConstants.CONSIGNER_FREETEXT,dictionary.get(ReportConstants.CONSIGNER));
            dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, dictionary.get(ReportConstants.NOTIFY_PARTY));
            dictionary.put(ReportConstants.CLIENT, client);
        }
    }

    public ShipmentDetails getShipment(Long Id)
    {
        return shipmentDao.findById(Id).get();
    }

    public ConsolidationDetails getConsolidation(Long Id)
    {
        return consolidationDetailsDao.findById(Id).get();
    }

    public Hbl getHbl(Long Id) {
        List<Hbl> hbls = hblDao.findByShipmentId(Id);
        if(hbls != null && !hbls.isEmpty())
            return hbls.get(0);
        return null;
    }

    public void populateConsolidationFields(ConsolidationDetails consolidation, Map<String, Object> dictionary) {
        if (consolidation == null) {
            return;
        }

        Parties sendingAgent = consolidation.getSendingAgent();
        Parties receivingAgent = consolidation.getReceivingAgent();
        Parties creditor = consolidation.getCreditor();
        ArrivalDepartureDetails arrivalDetails = consolidation.getArrivalDetails();

        UnlocationsResponse lastForeignPort = null;
        List<Object> criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                arrivalDetails.getLastForeignPort()
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            lastForeignPort = unlocationsResponse.get(0);

        dictionary.put(ReportConstants.CARGO_CLOSING_TIME, consolidation.getCargoClosingTime());
        dictionary.put(ReportConstants.DOCS_CLOSING_TIME, consolidation.getDocsClosingTime());
        dictionary.put(ReportConstants.MASTER_BILL, consolidation.getBol());
        List<String> exportAgentAddress = new ArrayList<>();
        List<String> importAgentAddress = new ArrayList<>();
        if(sendingAgent != null)
        {
            Map<String, Object> addressData = sendingAgent.getAddressData();
            if(addressData != null)
            {
                exportAgentAddress = ReportHelper.getOrgAddressWithPhoneEmail(addressData.get("CompanyName").toString(), addressData.get("Address1").toString(),
                        addressData.get("Address2").toString(),
                        ReportHelper.getCityCountry(addressData.get("City").toString(), addressData.get("Country").toString()),
                        null, addressData.get("ContactPhone").toString(),
                        addressData.get("Zip_PostCode").toString()
                );
                dictionary.put(ReportConstants.SENDING_AGENT_NAME, addressData.get("CompanyName"));
            }
            Map<String, Object> orgData = sendingAgent.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.SENDING_AGENT_LOCAL_NAME, orgData.get("LocalName"));
            }
        }
        if(receivingAgent != null)
        {
            Map<String, Object> addressData = receivingAgent.getAddressData();
            if(addressData != null)
            {
                importAgentAddress = ReportHelper.getOrgAddressWithPhoneEmail(addressData.get("CompanyName").toString(), addressData.get("Address1").toString(),
                        addressData.get("Address2").toString(),
                        ReportHelper.getCityCountry(addressData.get("City").toString(), addressData.get("Country").toString()),
                        null, addressData.get("ContactPhone").toString(),
                        addressData.get("Zip_PostCode").toString()
                );
                dictionary.put(ReportConstants.RECEIVING_AGENT_NAME, addressData.get("CompanyName"));
            }
            Map<String, Object> orgData = receivingAgent.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.RECEIVING_AGENT_LOCAL_NAME, orgData.get("LocalName"));
            }
        }

        List<String> exportAgentFreeTextAddress;
        List<String> importAgentFreeTextAddress;
        if (consolidation.getIsSendingAgentFreeTextAddress())
        {
            exportAgentFreeTextAddress = ReportHelper.getAddressList(consolidation.getSendingAgentFreeTextAddress());
        }
        else
        {
            exportAgentFreeTextAddress = exportAgentAddress;
        }

        if (consolidation.getIsReceivingAgentFreeTextAddress())
        {
            importAgentFreeTextAddress = ReportHelper.getAddressList(consolidation.getReceivingAgentFreeTextAddress());
        }
        else
        {
            importAgentFreeTextAddress = importAgentAddress;
        }
        if (Objects.equals(consolidation.getShipmentType(), "EXP"))
        {
            dictionary.put(ReportConstants.EXPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, exportAgentFreeTextAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, importAgentFreeTextAddress);
        }
        else
        {
            dictionary.put(ReportConstants.EXPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, importAgentFreeTextAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, exportAgentFreeTextAddress);
        }

        dictionary.put(ReportConstants.SHIPMENT_TYPE, consolidation.getShipmentType());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, !StringUtility.isEmpty(consolidation.getShipmentType())
                ? consolidation.getShipmentType().toUpperCase().charAt(0) : null);

        dictionary.put(ReportConstants.CONSOL_VESSEL_NAME, consolidation.getCarrierDetails().getVessel());
        dictionary.put(ReportConstants.CONSOL_VOYAGE, consolidation.getCarrierDetails().getVoyage());
        if(creditor != null)
        {
            Map<String, Object> orgData = creditor.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.CREDITOR, Arrays.asList(orgData.get("FullName"), creditor.getAddressData() != null ? creditor.getAddressData().get("Address1") : null));
                dictionary.put(ReportConstants.CREDITOR_LOCAL_NAME, orgData.get("LocalName"));
            }
        }
        dictionary.put(ReportConstants.CONSOL_ETA, consolidation.getCarrierDetails().getEta());
        dictionary.put(ReportConstants.CONSOL_ETD, consolidation.getCarrierDetails().getEtd());
        dictionary.put(ReportConstants.CONSOL_ATA, consolidation.getCarrierDetails().getAta());
        dictionary.put(ReportConstants.CONSOL_ATD, consolidation.getCarrierDetails().getAtd());

        dictionary.put(ReportConstants.CONSOL_DATE_OF_DEPARTURE, dictionary.get(ReportConstants.CONSOL_ATD) != null ? dictionary.get(ReportConstants.CONSOL_ATD) : dictionary.get(ReportConstants.CONSOL_ETD));

        if(arrivalDetails != null)
        {
            dictionary.put(ReportConstants.LAST_FOREIGN_PORT, lastForeignPort != null ? lastForeignPort.getName() : null);
            dictionary.put(ReportConstants.LAST_FORGEIN_PORT_COUNTRY, lastForeignPort != null ? lastForeignPort.getCountry() : null);
            dictionary.put(ReportConstants.DEST_PORT_NAME, lastForeignPort != null ? lastForeignPort.getName() : null);
            dictionary.put(ReportConstants.DEST_PORT_COUNTRY, lastForeignPort != null ? lastForeignPort.getCountry() : null);
        }
        dictionary.put(ReportConstants.MSN_NUMBER, consolidation.getMsnNumber());
        dictionary.put(ReportConstants.MRN_NUMBER, consolidation.getMrnNumber());
        dictionary.put(ReportConstants.MASTER_BILL_ISSUE_DATE, consolidation.getMasterBillIssueDate());
        dictionary.put(ReportConstants.CONSOL_REFERENCE_NUMBER, consolidation.getReferenceNumber());
        dictionary.put(ReportConstants.CONSOL_ADDITIONAL_TERMS, consolidation.getAdditionalTerms());
        dictionary.put(ReportConstants.CONSOL_FLIGHT_NUMBER, consolidation.getCarrierDetails().getFlightNumber());

        Parties notifyParty = null;
        List<Parties> consolidationAddresses = consolidation.getConsolidationAddresses();
        for (Parties consolidationAddress : consolidationAddresses) {
            if(Objects.equals(consolidationAddress.getType(), "Notify Party 1"))
            {
                notifyParty = consolidationAddress;
            }
        }
        if(notifyParty != null){
            Map<String, Object> addressData = notifyParty.getAddressData();
            List<String> consolNotifyPartyAddress = ReportHelper.getOrgAddressWithPhoneEmail(addressData.get("CompanyName").toString(), addressData.get("Address1").toString(),
                    addressData.get("Address2").toString(),
                    ReportHelper.getCityCountry(addressData.get("City").toString(), addressData.get("Country").toString()),
                    null, addressData.get("ContactPhone").toString(),
                    addressData.get("Zip_PostCode").toString()
            );
            dictionary.put(ReportConstants.CONSOL_NOTIFY_ADDRESS, consolNotifyPartyAddress);
        }
    }

    public void populateBlFields(Hbl hbl, Map<String, Object> dictionary)
    {
        if (hbl == null) return;
        HblPartyDto hblNotify = hbl.getHblNotifyParty().get(0);
        List<String> notify = ReportHelper.getOrgAddress(hblNotify.getName(), hblNotify.getAddress(), null, null, hblNotify.getEmail(), null);
        dictionary.put(ReportConstants.BL_NOTIFY_PARTY, notify);
        dictionary.put(ReportConstants.BL_NOTIFY_PARTY_CAPS, notify.stream().map(String::toUpperCase).collect(Collectors.toList()));
        HblDataDto hblDataDto = hbl.getHblData();
        List<String> consignor = ReportHelper.getOrgAddress(hblDataDto != null ? hblDataDto.getConsignorName() : null, hblDataDto != null ? hblDataDto.getConsignorAddress() : null, null, null, null, null);
        List<String> consignee = ReportHelper.getOrgAddress(hblDataDto != null ? hblDataDto.getConsigneeName() : null, hblDataDto != null ? hblDataDto.getConsigneeAddress() : null, null, null, null, null);
        dictionary.put(ReportConstants.CONSIGNER, consignor);
        dictionary.put(ReportConstants.CONSIGNEE, consignee);
        dictionary.put(ReportConstants.CONSIGNER_CAPS, consignor.stream().map(String::toUpperCase).collect(Collectors.toList()));
        dictionary.put(ReportConstants.CONSIGNEE_CAPS, consignee.stream().map(String::toUpperCase).collect(Collectors.toList()));
        dictionary.put(ReportConstants.HOUSE_BILL, hblDataDto.getHouseBill());
        dictionary.put(ReportConstants.BL_VESSEL_NAME, hblDataDto.getVesselName());
        dictionary.put(ReportConstants.BL_VOYAGE, hblDataDto.getVoyage());
        dictionary.put(ReportConstants.PORT_OF_LOADING, hblDataDto.getPortOfLoad());
        dictionary.put(ReportConstants.PORT_OF_DISCHARGE, hblDataDto.getPortOfDischarge());
        dictionary.put(ReportConstants.MARKS_N_NUMS, hblDataDto.getMarksAndNumbers());
        dictionary.put(ReportConstants.MARKS_N_NUMS_CAPS, hblDataDto.getMarksAndNumbers() != null ? hblDataDto.getMarksAndNumbers().toUpperCase() : null);
        dictionary.put(ReportConstants.PLACE_OF_RECEIPT, hblDataDto.getPlaceOfReceipt());
        dictionary.put(ReportConstants.PACKS, hblDataDto.getPackageCount());
        dictionary.put(ReportConstants.PACKS_UNIT, hblDataDto.getPackageType());
        MasterData masterData = getMasterListData(MasterDataType.PACKS_UNIT, hblDataDto.getPackageType());
        dictionary.put(ReportConstants.PACKS_UNIT_DESC, masterData != null && !masterData.getItemDescription().isEmpty() ? masterData.getItemDescription() : hblDataDto.getPackageType());
        dictionary.put(ReportConstants.DESCRIPTION, hblDataDto.getCargoDescription());
        dictionary.put(ReportConstants.DESCRIPTION_CAPS, hblDataDto.getCargoDescription() != null ? hblDataDto.getCargoDescription().toUpperCase() : null);
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, hblDataDto.getPlaceOfDelivery());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT, hblDataDto.getCargoNetWeight());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT_UNIT, hblDataDto.getCargoNetWeightUnit());
        dictionary.put(ReportConstants.FINAL_DESTINATION, hblDataDto.getFinalDestination());
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME, hblDataDto.getCargoGrossVolume());
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME_UNIT, hblDataDto.getCargoGrossVolumeUnit());
        dictionary.put(ReportConstants.CARGO_GROSS_WEIGHT, hblDataDto.getCargoGrossWeight());
        dictionary.put(ReportConstants.CARGO_GROSS_WEIGHT_UNIT, hblDataDto.getCargoGrossWeightUnit());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT, hblDataDto.getCargoNetWeight());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT_UNIT, hblDataDto.getCargoNetWeightUnit());
        dictionary.put(ReportConstants.CARGO_GROSS_PACKAGE_COUNT, hblDataDto.getPackageCount());
        dictionary.put(ReportConstants.CARGO_GROSS_PACKAGE_TYPE, hblDataDto.getPackageType());
        dictionary.put(ReportConstants.CARGO_GROSS_QUANTITY, hblDataDto.getQuantity());
        dictionary.put(ReportConstants.CARGO_GROSS_QUANTITY_CODE, hblDataDto.getQuantityCode());
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, hblDataDto.getPlaceOfDelivery());
        dictionary.put(ReportConstants.PLACE_OF_RECEIPT, hblDataDto.getPlaceOfReceipt());
        dictionary.put(ReportConstants.BL_COMMENTS, hblDataDto.getBlComments());
        dictionary.put(ReportConstants.BLComments, hblDataDto.getBlComments());
        dictionary.put(ReportConstants.BL_DELIVERY_AGENT, hblDataDto.getDeliveryAgent());
        dictionary.put(ReportConstants.BL_DELIVERY_AGENT_ADDRESS, hblDataDto.getDeliveryAgentAddress());
        dictionary.put(ReportConstants.BL_CARGO_TERMS_DESCRIPTION, hblDataDto.getCargoTermsDescription());
        dictionary.put(ReportConstants.BL_REMARKS_DESCRIPTION, hblDataDto.getBlRemarksDescription());
    }

    public void populateUserFields(UsersDto user, Map<String, Object> dictionary) {
        if (user == null) {
            return;
        }
        dictionary.put(ReportConstants.USER_FULLNAME, user.DisplayName);
        dictionary.put(ReportConstants.TENANT_NAME, user.TenantDisplayName);
        dictionary.put(ReportConstants.USER_NAME, user.Username);
        dictionary.put(ReportConstants.USER_EMAIL, user.Email);
        dictionary.put(ReportConstants.TENANT_CURRENCY, user.CompanyCurrency);
    }

    public MasterData getMasterListData(MasterDataType type, String ItemValue)
    {
        if (StringUtility.isEmpty(ItemValue)) return null;
        MasterListRequest masterListRequest = MasterListRequest.builder().ItemType(type.name()).ItemValue(ItemValue).build();
        List<MasterListRequest> masterListRequests = new ArrayList<>();
        masterListRequests.add(masterListRequest);
        V1DataResponse v1DataResponse = v1Service.fetchMultipleMasterData(masterListRequests);
        List<MasterData> masterData = jsonHelper.convertValueToList(v1DataResponse.entities, MasterData.class);
        if(masterData == null || masterData.isEmpty())
            return null;
        return masterData.get(0);
    }

    public List<ContainerCountByCode> getCountByContainerTypeCode(List<ShipmentContainers> commonContainers) {
        Map<String, Long> containerTypeCountMap = new HashMap<>();
        if (commonContainers != null) {
            for(ShipmentContainers container : commonContainers) {
                if (container.ContainerTypeCode != null) {
                    Long containerCount = container.getContainerCount();
                    if (!containerTypeCountMap.containsKey(container.ContainerTypeCode)) {
                        containerTypeCountMap.put(container.ContainerTypeCode, containerCount);
                        continue;
                    }
                    Long containers = containerTypeCountMap.get(container.ContainerTypeCode);
                    containers += containerCount;
                    containerTypeCountMap.put(container.ContainerTypeCode, containers);
                }
            }
        }

        List<ContainerCountByCode> containerCountByCode = new ArrayList<>();
        for(var entry : containerTypeCountMap.entrySet()) {
            ContainerCountByCode countByCode = new ContainerCountByCode();
            countByCode.ContainerTypeCode = entry.getKey();
            countByCode.ContainerCount = entry.getValue();
            containerCountByCode.add(countByCode);
        }
        return containerCountByCode;
    }

    public Awb getHawb(Long Id) {
        List<Awb> awb = awbRepository.findByShipmentId(Id);
        if(awb != null && !awb.isEmpty())
            return awb.get(0);
        return null;
    }

    public Awb getMawb(Long Id) {
        List<Awb> awb = awbRepository.findByConsolidationId(Id);
        if(awb != null && !awb.isEmpty())
            return awb.get(0);
        return null;
    }

    public static List<String> getFormattedDetails(String name, String address)
    {
        List<String> details = new ArrayList<>();
        details.add(name);
        String[] addressList = address.split("\r\n");
        addressList = Arrays.stream(addressList)
                .filter(Objects::nonNull)
                .map(String::trim)
                .filter(Predicate.isEqual("").negate())
                .toArray(String[]::new);
        details.addAll(Arrays.asList(addressList));
        return details;
    }

    public static String addCommas(BigDecimal amount)
    {
        if (amount == null) return null;
        return String.format("{0:n2}", amount);
    }

    public static String addCommas(String amount)
    {
        if (amount == null)
        {
            return null;
        }
        try{
            return String.format("{0:n2}", new BigDecimal(amount));
        }
        catch (Exception ex)
        {
            return amount;
        }
    }

    public static String appendZero(String value, int length){
        int size = value.length();
        for(int i=0; i<length-size; i++){
            value = "0" + value;
        }
        return value;
    }

    public static String twoDecimalPlacesFormat(String value)
    {
        if(StringUtility.isEmpty(value))
        {
            return value;
        }

        else
        {
            return String.format("##.00", value);
        }
    }

    public static String twoDecimalPlacesFormatDecimal(BigDecimal value)
    {
        if(value == null)
        {
            return "0.00";
        }
        return twoDecimalPlacesFormat(value.toString());
    }

    public static DateTimeFormatter GetDPWDateFormatOrDefault()
    {
        return DateTimeFormatter.ofPattern("MM/dd/yyyy");
    }

    public static String ConvertToDPWDateFormat(LocalDateTime date)
    {
        String strDate = "";
        if (date != null)
        {
            strDate = date.format(GetDPWDateFormatOrDefault());
        }
        return strDate;
    }
}
