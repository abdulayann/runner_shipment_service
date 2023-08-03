package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.AwbRequest;
import com.dpw.runner.shipment.services.dto.request.CreateAwbRequest;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.dpw.runner.shipment.services.utils.AwbUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class AwbService implements IAwbService {

    @Autowired
    private IAwbDao awbDao;

    @Autowired
    IShipmentDao shipmentDao;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    IPackingDao packingDao;

    @Autowired
    IMawbHawbLinkDao mawbHawbLinkDao;

    @Autowired
    private JsonHelper jsonHelper;

    private Integer totalPacks = 0;
    private List<String> attachedShipmentDescriptions = new ArrayList<>();
    private BigDecimal totalVolumetricWeightOfAwbPacks = new BigDecimal(0);

    public ResponseEntity<?> createAwb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for AWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getShipmentId() == null) {
            log.error("Shipment Id can't be null or empty in create AWB Request");
            throw new ValidationException("Shipment Id can't be null or empty in Create AWB Request");
        }

        Awb awb = new Awb();
        try {
            awb = awbDao.save(generateAwb(request));
            log.info("AWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    public ResponseEntity<?> updateAwb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AwbRequest request = (AwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for AWB update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.error("Request Id is null for AWB update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Awb> oldEntity = awbDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("AWB is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Awb awb = convertRequestToEntity(request);
        try {
            awb = awbDao.save(awb);
            log.info("Updated the AWB Shipment Info for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for AWB list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Awb>, Pageable> tuple = fetchData(request, Awb.class);
            Page<Awb> awbPage = awbDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("AWB list retrieved successfully for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(awbPage.getContent()),
                    awbPage.getTotalPages(),
                    awbPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for AWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for AWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Awb> awb = awbDao.findById(id);
            if (!awb.isPresent()) {
                log.debug("AWB is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("AWB fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            AwbResponse response = convertEntityToDto(awb.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> createMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for MAWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getConsolidationId() == null) {
            log.error("Consolidation Id can't be null or empty in create MAWB Request");
            throw new ValidationException("Shipment Id can't be null or empty in Create MAWB Request");
        }

        Awb awb = new Awb();
        try {
            // fetch consolidation info
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId()).get();

            // save awb details
            awb = awbDao.save(generateMawb(request, consolidationDetails));

            // map mawb and hawb affter suuccessful save
            LinkHawbMawb(consolidationDetails, awb.getId());
            log.info("MAWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    public ResponseEntity<?> updateGoodsAndPacksForMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Update Goods And Packs For Mawb for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getShipmentId() == null) {
            log.error("Shipment Id can't be null or empty in Update Goods And Packs For Mawb Request");
            throw new ValidationException("Shipment Id can't be null or empty in Update Goods And Packs For Mawb");
        }

        Awb awb = new Awb();
        try {
            // awb = awbDao.save(generateAwb(request));
            updateGoodsAndPacks(request); //TODO
            log.info("Update Goods And Packs For Mawb successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    private void updateGoodsAndPacks(CreateAwbRequest request) {
        // TODO
    }

    private AwbResponse convertEntityToDto(Awb awbShipmentInfo) {
        return jsonHelper.convertValue(awbShipmentInfo, AwbResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Awb> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(awbShipmentInfo -> {
            responseList.add(convertEntityToDto(awbShipmentInfo));
        });
        return responseList;
    }

    private Awb convertRequestToEntity(AwbRequest request) {
        return jsonHelper.convertValue(request, Awb.class);
    }

    private Awb generateMawb(CreateAwbRequest request, ConsolidationDetails consolidationDetails) {
        // validate the request
        AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails);

        var awbPackingInfo = generateMawbPackingInfo(consolidationDetails);
        // generate Awb Entity
        return Awb.builder()
                .awbShipmentInfo(generateMawbShipmentInfo(consolidationDetails, request))
                .awbNotifyPartyInfo(generateMawbNotifyPartyinfo(consolidationDetails, request))
                .awbRoutingInfo(generateMawbRoutingInfo(consolidationDetails, request))
                .awbCargoInfo(generateMawbCargoInfo(consolidationDetails, request, awbPackingInfo))
                .awbOtherInfo(generateMawbOtherInfo(consolidationDetails, request))
                .awbGoodsDescriptionInfo(generateMawbGoodsDescriptionInfo(consolidationDetails, request))
                .awbPackingInfo(awbPackingInfo)
                .build();
    }

    private AwbShipmentInfo generateMawbShipmentInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        AwbShipmentInfo awbShipmentInfo = new AwbShipmentInfo();
        awbShipmentInfo.setEntityId(consolidationDetails.getId());
        awbShipmentInfo.setEntityType(request.getAwbType());
        // awbShipmentInfo.setShipperName(consolidationDetails.getSendingAgentName()); // missing
        awbShipmentInfo.setAwbNumber(consolidationDetails.getMAWB());
        awbShipmentInfo.setFirstCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
        awbShipmentInfo.setShipperAddress(consolidationDetails.getSendingAgentFreeTextAddress());
        awbShipmentInfo.setShipperReferenceNumber(consolidationDetails.getAgentReference());
        // awbShipmentInfo.setConsigneeName(consolidationDetails.getReceivingAgentName()); //missing
        awbShipmentInfo.setConsigneeAddress(consolidationDetails.getReceivingAgentFreeTextAddress());
        // awbShipmentInfo.setConsigneeReferenceNumber(consolidationDetails.getReceivingAgentId()); //missing
        // AwbUtility.getConsolidationForwarderDetails(uow, consolidationRow, awbShipmentInfo, awbOtherInfoRow, awbCargoInfo); TODO
        // awbShipmentInfo.setOriginAirport(consolidationDetails.setOriginPort()); // missing
        // awbShipmentInfo.setDestinationAirport(consolidationDetails.setDestinationPort()); // missing

        return awbShipmentInfo;
    }

    private List<AwbNotifyPartyInfo> generateMawbNotifyPartyinfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        if (consolidationDetails.getConsolidationAddresses() != null &&
                consolidationDetails.getConsolidationAddresses().size() > 0) {
            List<AwbNotifyPartyInfo> notifyPartyList = new ArrayList<>();
            for (var party : consolidationDetails.getConsolidationAddresses()) {
                if (party.getOrgData().get("Type") == "Notify Part 1" ||
                        party.getOrgData().get("Type") == "Notify Part 2" ||
                        party.getOrgData().get("Type") == "Notify Part 3") {
                    AwbNotifyPartyInfo notifyPartyInfo = new AwbNotifyPartyInfo();
                    var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
                    notifyPartyInfo.setName(name == null ? name : name.toUpperCase());
                    notifyPartyInfo.setAddress(AwbUtility.constructAddress(party.getAddressData()).toUpperCase());
                    notifyPartyInfo.setEntityId(consolidationDetails.getId());
                    notifyPartyInfo.setEntityType(request.getAwbType());
                    // notifyPartyInfo.setAddressId(shipmentNotifyParty.getAddressData()); // field missing: AddressId
                    notifyPartyInfo.setNotifyOrgId(consolidationDetails.getId());
                    notifyPartyList.add(notifyPartyInfo);
                }
            }

            return notifyPartyList;
        }
        return null;
    }

    private List<AwbRoutingInfo> generateMawbRoutingInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        if (consolidationDetails.getCarrierDetails() != null &&
                consolidationDetails.getCarrierDetails().getOriginPort() != null &&
                consolidationDetails.getCarrierDetails().getDestinationPort() != null
        ) {
            AwbRoutingInfo routingInfo = new AwbRoutingInfo();
//            routingInfo.setOrigin(consolidationDetails.getCarrierDetails().getOriginPort()); // field missing: POLId
//            routingInfo.setDestination(consolidationDetails.getCarrierDetails().getDestinationPort()); // field missing PODId:
            routingInfo.setOriginPortName(consolidationDetails.getCarrierDetails().getOriginPort());
            routingInfo.setDestinationPortName(consolidationDetails.getCarrierDetails().getDestinationPort());
            routingInfo.setByCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
            routingInfo.setFlightNumber(consolidationDetails.getCarrierDetails().getFlightNumber());
            routingInfo.setEntityId(consolidationDetails.getId());
            routingInfo.setEntityType(request.getAwbType());
            return Arrays.asList(routingInfo);
        }
        return null;
    }

    private AwbCargoInfo generateMawbCargoInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList) {
        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        String concatenatedGoodsDesc = ""; //TODO from consoleshipment mapping
        if (attachedShipmentDescriptions.size() > 0) {
            concatenatedGoodsDesc = String.join(",", attachedShipmentDescriptions);
        }
        awbCargoInfo.setNtrQtyGoods(AwbUtility.generateNatureAndQuantFieldsForConsolMawb(concatenatedGoodsDesc, totalVolumetricWeightOfAwbPacks, awbPackingList));
        awbCargoInfo.setEntityId(consolidationDetails.getId());
        awbCargoInfo.setEntityType(request.getAwbType());
//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        // awbCargoInfo.setCurrency(tenant.CurrencyCode); // get currency from tenant info
        // awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterListTypes.MAWBGeneration)); // field missing
        awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        return awbCargoInfo;
    }

    private List<AwbGoodsDescriptionInfo> generateMawbGoodsDescriptionInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = new AwbGoodsDescriptionInfo();
        awbGoodsDescriptionInfo.setEntityId(consolidationDetails.getId());
        awbGoodsDescriptionInfo.setEntityType(request.getAwbType());
        awbGoodsDescriptionInfo.setGrossWtUnit("KG");
        return Arrays.asList(awbGoodsDescriptionInfo);
    }

    private AwbOtherInfo generateMawbOtherInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        AwbOtherInfo awbOtherInfo = new AwbOtherInfo();
        awbOtherInfo.setEntityId(consolidationDetails.getId());
        awbOtherInfo.setEntityType(request.getAwbType());
        // awbOtherInfo.setShipper(consolidationDetails.getSendingAgentName()); //missing
        awbOtherInfo.setExecutedOn(LocalDateTime.now());
        return awbOtherInfo;
    }

    private void LinkHawbMawb(ConsolidationDetails consolidationDetails, Long mawbId) {
        for (var consoleShipment : consolidationDetails.getShipmentsList()) {
            if (consoleShipment.getId() != null) {
                Awb awb = awbDao.findByShipmentId(consoleShipment.getId()).stream().findFirst().get();
                if (awb == null) {
                    throw new ValidationException("To Generate Mawb, Please create Hawb for all the shipments attached");
                }

                MawbHawbLink mawbHawblink = new MawbHawbLink();
                mawbHawblink.setHawbId(awb.getId());
                mawbHawblink.setHawbId(mawbId);
                mawbHawbLinkDao.save(mawbHawblink);
            }
        }
    }

    private List<AwbPackingInfo> generateMawbPackingInfo(ConsolidationDetails consolidationDetails) {
        List<AwbPackingInfo> awbPackingList = new ArrayList<>();
        List<AwbGoodsDescriptionInfo> awbGoodsDescList = new ArrayList<>();
        List<Long> attachedHawbIds = new ArrayList<>();
        List<AwbPackingInfo> hawbPacksLinkedToMawb = new ArrayList<>();

        if (consolidationDetails.getShipmentsList().size() > 0) {
            for (var consoleShipment : consolidationDetails.getShipmentsList()) {
                if (!StringUtility.isEmpty(consoleShipment.getGoodsDescription())) {
                    attachedShipmentDescriptions.add(consoleShipment.getGoodsDescription());
                }

                var awbList = awbDao.findByShipmentId(consoleShipment.getId());
                if (awbList == null || awbList.size() == 0) {
                    throw new ValidationException("To Generate Mawb, Please create Hawb for all the shipments attached");
                }

                var awb = awbList.stream().findFirst().get();
                if (awb.getAwbPackingInfo() != null && awb.getAwbPackingInfo().size() > 0) {
                    for (var awbPack : awb.getAwbPackingInfo()) {
                        if (awbPack.getVolume() != null && !StringUtility.isEmpty(awbPack.getVolumeUnit()) &&
                                awbPack.getVolumeUnit() == "M3") {
                            totalVolumetricWeightOfAwbPacks.add(awbPack.getVolume());
                        }
                        hawbPacksLinkedToMawb.add(awbPack);
                    }
                }
            }
            Double factor = Constants.FACTOR_VOL_WT;
            totalVolumetricWeightOfAwbPacks.multiply(new BigDecimal(factor));
        }
        return hawbPacksLinkedToMawb;
    }

    private Awb generateAwb(CreateAwbRequest request) {
        // fetch sehipment info
        ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();

        // fetch all packings
        ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", request.getShipmentId(), "=");
        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
        Page<Packing> packingPage = packingDao.findAll(pair.getLeft(), pair.getRight());
        List<Packing> packings = packingPage.getContent();

        // Generate HAWB Number if restrictHBlGeneration && numberSequencing
        // shipmentDetails.setHouseBill(generateCustomizedBLNumber(shipmentDetails)); //TODO - implement logic to generate house bill

        // validate the request
        AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails);

        var awbPackingInfo = generateAwbPackingInfo(shipmentDetails, packings);
        // generate Awb Entity
        return Awb.builder()
                .awbShipmentInfo(generateAwbShipmentInfo(shipmentDetails, request))
                .awbNotifyPartyInfo(generateAwbNotifyPartyinfo(shipmentDetails, request))
                .awbRoutingInfo(generateAwbRoutingInfo(shipmentDetails, request))
                .awbCargoInfo(generateAwbCargoInfo(shipmentDetails, request, awbPackingInfo))
                .awbOtherInfo(generateAwbOtherInfo(shipmentDetails, request))
                .awbGoodsDescriptionInfo(generateAwbGoodsDescriptionInfo(shipmentDetails, request))
                .awbPackingInfo(awbPackingInfo)
                .shipmentId(shipmentDetails.getId())
                .build();
    }

    private AwbShipmentInfo generateAwbShipmentInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        AwbShipmentInfo awbShipmentInfo = new AwbShipmentInfo();
        awbShipmentInfo.setEntityId(shipmentDetails.getId());
        awbShipmentInfo.setEntityType(request.getAwbType());
        awbShipmentInfo.setAwbNumber(shipmentDetails.getHouseBill());
        var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME));
        awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
        var shipperAddress = AwbUtility.constructAddress(shipmentDetails.getConsigner().getAddressData());
        awbShipmentInfo.setShipperAddress(shipperAddress == null ? shipperAddress : shipperAddress.toUpperCase());
        var consigneeName = StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.FULLNAME));
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        var consigneeAddress = AwbUtility.constructAddress(shipmentDetails.getConsignee().getAddressData());
        awbShipmentInfo.setConsigneeAddress(consigneeAddress == null ? consigneeAddress : consigneeAddress.toUpperCase());

        awbShipmentInfo.setConsigneeReferenceNumber(shipmentDetails.getConsignee().getId().toString());
        awbShipmentInfo.setOriginAirport(shipmentDetails.getCarrierDetails().getOriginPort());
        awbShipmentInfo.setDestinationAirport(shipmentDetails.getCarrierDetails().getDestinationPort());
        awbShipmentInfo.setFirstCarrier(shipmentDetails.getCarrierDetails().getShippingLine());

        // awbShipmentInfo.setIataCode(tenant.AgentIATACode); // field missing
        // awbShipmentInfo.setAgentCASSCode(tenant.AgentCASSCode); // field missing
        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType() == Constants.FORWARDING_AGENT) {
                var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
                var issuingAgentAddress = AwbUtility.constructAddress(orgRow.getAddressData());
                awbShipmentInfo.setIssuingAgentAddress(issuingAgentAddress == null ? issuingAgentAddress : issuingAgentAddress.toUpperCase());

                awbShipmentInfo.setIataCode(StringUtility.isEmpty(awbShipmentInfo.getIataCode())
                        ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                        : awbShipmentInfo.getIataCode());
                awbShipmentInfo.setAgentCASSCode(StringUtility.isEmpty(awbShipmentInfo.getAgentCASSCode())
                        ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_CASS_CODE))
                        : awbShipmentInfo.getAgentCASSCode());
                // awbOtherInfoRow.setExecutedAt(getCityId(orgRow.OrgId)); // fetch from master data
                // awbCargoInfo.CustomOriginCode(getCountryCode(orgRow.OrgCountry)); // fetch from master data
            }
        }

        return awbShipmentInfo;
    }

    private List<AwbNotifyPartyInfo> generateAwbNotifyPartyinfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        if (shipmentDetails.getAdditionalDetails() != null &&
                shipmentDetails.getAdditionalDetails().getNotifyParty() != null &&
                shipmentDetails.getAdditionalDetails().getNotifyParty().getId() != null) {
            var shipmentNotifyParty = shipmentDetails.getAdditionalDetails().getNotifyParty();
            AwbNotifyPartyInfo notifyPartyInfo = new AwbNotifyPartyInfo();
            var name = StringUtility.convertToString(shipmentNotifyParty.getOrgData().get(PartiesConstants.FULLNAME));
            notifyPartyInfo.setName(name == null ? name : name.toUpperCase());
            notifyPartyInfo.setAddress(AwbUtility.constructAddress(shipmentNotifyParty.getAddressData()).toUpperCase());
            notifyPartyInfo.setEntityId(shipmentDetails.getId());
            notifyPartyInfo.setEntityType(request.getAwbType());
            // notifyPartyInfo.setAddressId(shipmentNotifyParty.getAddressData()); // field missing: AddressId
            notifyPartyInfo.setNotifyOrgId(shipmentNotifyParty.getId());
            return Arrays.asList(notifyPartyInfo);
        }
        return null;
    }

    private List<AwbRoutingInfo> generateAwbRoutingInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        if (shipmentDetails.getCarrierDetails() != null &&
                shipmentDetails.getCarrierDetails().getOriginPort() != null &&
                shipmentDetails.getCarrierDetails().getDestinationPort() != null
        ) {
            var flightDate = request.getAwbType() == Constants.DMAWB ? shipmentDetails.getCarrierDetails().getEtd() : null;
            AwbRoutingInfo routingInfo = new AwbRoutingInfo();
//            routingInfo.setOrigin(shipmentDetails.getCarrierDetails().getOriginPort()); // field missing: POLId
//            routingInfo.setDestination(shipmentDetails.getCarrierDetails().getDestinationPort()); // field missing PODId:
            routingInfo.setOriginPortName(shipmentDetails.getCarrierDetails().getOriginPort());
            routingInfo.setDestinationPortName(shipmentDetails.getCarrierDetails().getDestinationPort());
            routingInfo.setByCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
            routingInfo.setFlightNumber(shipmentDetails.getCarrierDetails().getFlightNumber());
            routingInfo.setFlightDate(flightDate);
            routingInfo.setEntityId(shipmentDetails.getId());
            routingInfo.setEntityType(request.getAwbType());
            return Arrays.asList(routingInfo);
        }
        return null;
    }

    private AwbCargoInfo generateAwbCargoInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList) {
        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        awbCargoInfo.setNtrQtyGoods(AwbUtility.generateNatureAndQuantGoodsField(shipmentDetails.getGoodsDescription(), shipmentDetails.getVolumetricWeight(), awbPackingList));
        awbCargoInfo.setEntityId(shipmentDetails.getId());
        awbCargoInfo.setEntityType(request.getAwbType());
//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        // awbCargoInfo.setCurrency(tenant.CurrencyCode); // get currency from tenant info
        // awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterListTypes.HAWBGeneration)); // field missing
        awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        return awbCargoInfo;
    }

    private AwbOtherInfo generateAwbOtherInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        AwbOtherInfo awbOtherInfo = new AwbOtherInfo();
        awbOtherInfo.setEntityId(shipmentDetails.getId());
        awbOtherInfo.setEntityType(request.getAwbType());
        var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME));
        awbOtherInfo.setShipper(shipperName == null ? null : shipperName.toUpperCase());
        awbOtherInfo.setExecutedOn(LocalDateTime.now());
        return awbOtherInfo;
    }

    private List<AwbGoodsDescriptionInfo> generateAwbGoodsDescriptionInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = new AwbGoodsDescriptionInfo();
        awbGoodsDescriptionInfo.setEntityId(shipmentDetails.getId());
        awbGoodsDescriptionInfo.setEntityType(request.getAwbType());
        awbGoodsDescriptionInfo.setGrossWt(shipmentDetails.getWeight());
        awbGoodsDescriptionInfo.setGrossWtUnit(shipmentDetails.getWeightUnit());
        awbGoodsDescriptionInfo.setPiecesNo(totalPacks);
        awbGoodsDescriptionInfo.setChargeableWt(shipmentDetails.getChargable() != null ?
                AwbUtility.roundOffAirShipment((double) shipmentDetails.getChargable().doubleValue()) : null);
        return Arrays.asList(awbGoodsDescriptionInfo);
    }

    private List<AwbPackingInfo> generateAwbPackingInfo(ShipmentDetails shipmentDetails, List<Packing> packings) {
        if (packings != null && packings.size() > 0) {
            List<AwbPackingInfo> awbPackingList = new ArrayList<>();
            // Integer totalPacks = 0;
            for (var packing : packings) {
                AwbPackingInfo awbPacking = new AwbPackingInfo();
                awbPacking.setDgGoodsId(packing.getDGGoodsId());
                awbPacking.setDgSubstanceId(packing.getDGSubstanceId());
                awbPacking.setPacks(packing.getPacks());
                awbPacking.setPacksType(packing.getPacksType());
                awbPacking.setContainerNumber(packing.getContainerNumber());
                awbPacking.setWeight(packing.getWeight());
                awbPacking.setWeightUnit(packing.getWeightUnit());
                awbPacking.setVolume(packing.getVolume());
                awbPacking.setVolumeUnit(packing.getVolumeUnit());
                awbPacking.setInspections(packing.getInspections());
                awbPacking.setOrigin(packing.getOrigin());
                awbPacking.setCommodity(packing.getCommodity());
                awbPacking.setPackingOrder(packing.getPackingOrder());
                awbPacking.setLength(packing.getLength());
                awbPacking.setLengthUnit(packing.getLengthUnit());
                awbPacking.setWidth(packing.getWidth());
                awbPacking.setWidthUnit(packing.getWidthUnit());
                awbPacking.setHeight(packing.getHeight());
                awbPacking.setHeightUnit(packing.getHeightUnit());
                awbPacking.setMarksnNums(packing.getMarksnNums());
                awbPacking.setFlashPoint(packing.getFlashPoint());
                awbPacking.setUndgContact(packing.getUNDGContact());
                awbPacking.setIsTemperatureControlled(packing.getIsTemperatureControlled());
                awbPacking.setMinTemp(packing.getMinTemp());
                awbPacking.setMinTempUnit(packing.getMinTempUnit());
                awbPacking.setHsCode(packing.getHSCode());
                awbPacking.setCountryCode(packing.getCountryCode());
                awbPacking.setGoodsDescription(packing.getGoodsDescription() == null ? null : packing.getGoodsDescription().toUpperCase());
                awbPacking.setReferenceNumber(packing.getReferenceNumber());
                awbPacking.setDgClass(packing.getDGClass());
                awbPacking.setHazardous(packing.getHazardous());
                awbPacking.setCommodityId(packing.getCommodityId());
                awbPacking.setNetWeight(packing.getNetWeight());
                awbPacking.setNetWeightUnit(packing.getNetWeightUnit());
                awbPacking.setVolumeWeight(packing.getVolumeWeight());
                awbPacking.setVolumeWeightUnit(packing.getVolumeWeightUnit());
                awbPacking.setAwbNumber(shipmentDetails.getHouseBill());
                totalPacks += Integer.parseInt(packing.getPacks());
                awbPackingList.add(awbPacking);
            }

            return awbPackingList;
        }
        return null;
    }

}