package com.dpw.runner.shipment.services.service.v1.util;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.UserWithPermissionRequestV1;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitFromV1Response;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.request.AddressTranslationRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreditLimitValidateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TenantDetailsByListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;


@Component
@Slf4j
public class V1ServiceUtil {
    @Autowired
    INotesDao notesDao;
    @Autowired
    IV1Service v1Service;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ModelMapper modelMapper;

    public CreateBookingModuleInV1 createBookingRequestForV1(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled, UUID shipmentGuid) {
        return CreateBookingModuleInV1.builder()
                .IsP100Booking(Boolean.TRUE)
                .Entity(createEntity(customerBooking, isShipmentEnabled, isBillingEnabled))
                .ShipmentGuid(StringUtility.convertToString(shipmentGuid))
                .build();
    }

    private CreateBookingModuleInV1.BookingEntity createEntity(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled) {
        var carrierDetails = Optional.ofNullable(customerBooking.getCarrierDetails());
        List<Notes> notes = notesDao.findByEntityIdAndEntityType(customerBooking.getId(), "CustomerBooking");
        return CreateBookingModuleInV1.BookingEntity.builder()
                .Voyage(customerBooking.getCarrierDetails() != null ? customerBooking.getCarrierDetails().getVoyage() : null)
                .ContractId(customerBooking.getContractId())
                .NotifyPartyCode(Objects.isNull(customerBooking.getNotifyParty()) ? null : customerBooking.getNotifyParty().getOrgCode())
                .NotifyPartyAddressCode(Objects.isNull(customerBooking.getNotifyParty()) ? null : customerBooking.getNotifyParty().getAddressCode())
                .Carrier(customerBooking.getCarrierDetails() != null ? customerBooking.getCarrierDetails().getShippingLine() : null)
                .FlightNumber(customerBooking.getCarrierDetails() != null ? customerBooking.getCarrierDetails().getFlightNumber() : null)
                .VesselName(customerBooking.getCarrierDetails() != null ? customerBooking.getCarrierDetails().getVessel() : null)
                .Packs(Objects.isNull(customerBooking.getQuantity()) ? null : Long.valueOf(customerBooking.getQuantity()))
                .PacksUnit(customerBooking.getQuantityUnit())
                .Weight(customerBooking.getGrossWeight())
                .WeightUnit(customerBooking.getGrossWeightUnit())
                .VolumeWeight(customerBooking.getWeightVolume())
                .WeightVolumeUnit(customerBooking.getVolumeUnit())
                .Volume(customerBooking.getVolume())
                .VolumeUnit(customerBooking.getVolumeUnit())
                .ReferenceNo(customerBooking.getBookingNumber())
                .CreatedDate(customerBooking.getBookingDate() != null ? DateTimeFormatter.ofPattern(CustomerBookingConstants.DATE_FORMAT).format(customerBooking.getBookingDate()) : null)
                .ClientCode(customerBooking.getCustomer() != null ? customerBooking.getCustomer().getOrgCode() : null)
                .ClientAddressShortCode(customerBooking.getCustomer() != null ? customerBooking.getCustomer().getAddressCode() : null)
                .ConsignerCode(customerBooking.getConsignor() != null ? customerBooking.getConsignor().getOrgCode() : null)
                .ConsignerAddressCode(customerBooking.getConsignor() != null ? customerBooking.getConsignor().getAddressCode() : null)
                .ConsigneeCode(customerBooking.getConsignee() != null ? customerBooking.getConsignee().getOrgCode() : null)
                .ConsigneeAddressCode(customerBooking.getConsignee() != null ? customerBooking.getConsignee().getAddressCode() : null)
                .OriginCode(carrierDetails.map(c -> c.getOrigin()).orElse(null))
                .DestinationCode(carrierDetails.map(c -> c.getDestination()).orElse(null))
                .originPortCode(carrierDetails.map(c -> c.getOriginPort()).orElse(null))
                .DestinationPortCode(carrierDetails.map(c -> c.getDestinationPort()).orElse(null))
                .ConsolidationType(CustomerBookingConstants.STD)
                .TransportMode(customerBooking.getTransportType())
                .ContainerType(customerBooking.getCargoType())
                .CustomShipmentType(customerBooking.getDirection())
                .Incoterm(customerBooking.getIncoTerms())
                .ServiceMode(customerBooking.getServiceMode())
                .IsShipmentCreateEnabled(isShipmentEnabled)
                .IsConsolidationCreateEnabled(customerBooking.getCargoType().equals(CustomerBookingConstants.FCL) && isShipmentEnabled)
                .IsBillCreateEnabled(isBillingEnabled)
                .BookingType(CustomerBookingConstants.ONLINE)
                .Status(CustomerBookingConstants.ONE)
                .FmcTlcId(customerBooking.getFmcTlcId())
                .ClientCountryFilter(customerBooking.getClientCountry())
                .ConsignorCountryFilter(customerBooking.getConsignorCountry())
                .ConsigneeCountryFilter(customerBooking.getConsigneeCountry())
                .NotifyPartyCountryFilter(customerBooking.getNotifyPartyCountry())
                .SalesBranch(customerBooking.getSalesBranch())
                .PrimarySalesAgentEmail(customerBooking.getPrimarySalesAgentEmail())
                .SecondarySalesAgentEmail(customerBooking.getSecondarySalesAgentEmail())
                .QuoteContainers(createContainers(customerBooking.getContainersList()))
                .RoutingList(createRoutingList(customerBooking.getRoutingList()))
                .Loosecargos(createLooseCarges(customerBooking.getPackingList()))
                .OrgDetails(null)
                .BillCharges(createQuoteCharges(customerBooking.getBookingCharges()))
                .CustomerBookingNoteList(createNotes(notes))
                .LastTransactionLoadJson(getLastLoadJson(customerBooking.getContainersList()))
                .build();
    }

    private  static List<CreateBookingModuleInV1.BookingEntity.Notes> createNotes(List<Notes>notes){
        if (notes == null) return null;
        return notes.stream().filter(Objects::nonNull).map(note ->
                CreateBookingModuleInV1.BookingEntity.Notes.builder()
                        .AssignedTo(note.getAssignedTo())
                        .Label(note.getLabel())
                        .Text(note.getText())
                        .InsertUserDisplayName(note.getCreatedBy())
                        .IsPublic(note.getIsPublic())
                        .InsertDate(note.getCreatedAt() != null ? DateTimeFormatter.ofPattern(CustomerBookingConstants.DATE_TIME_FORMAT).format(note.getCreatedAt()) : null)
                        .build()).toList();
    }
    private static List<CreateBookingModuleInV1.BookingEntity.BillCharge> createQuoteCharges(List<BookingCharges> bookingCharges) {
        if (bookingCharges == null) return null;
        return bookingCharges.stream().filter(Objects::nonNull).map(bc ->
                CreateBookingModuleInV1.BookingEntity.BillCharge.builder()
                        .OverseasSellCurrency(bc.getOverseasSellCurrency())
                        .ChargeTypeCode(bc.getChargeType())
                        .EstimatedRevenue(bc.getEstimatedRevenue())
                        .LocalSellAmount(bc.getLocalSellAmount())
                        .LocalSellCurrency(bc.getLocalSellCurrency())
                        .NoGST(bc.getNoGST())
                        .OverseasSellAmount(bc.getOverseasSellAmount())
                        .SellExchange(bc.getSellExchange() != null && !bc.getSellExchange().equals(BigDecimal.ZERO)? BigDecimal.ONE.divide(bc.getSellExchange(),15, RoundingMode.HALF_UP) : bc.getSellExchange())
                        .TaxPercentage(bc.getTaxPercentage())
                        .ContainersGuid(createContainersGuid(bc))
                        .RevenueLineTotal(bc.getRevenueLineTotal())
                        .OverseasTax(bc.getOverseasTax())
                        .TaxType1(bc.getTaxType1())
                        .TaxType2(bc.getTaxType2())
                        .TaxType3(bc.getTaxType3())
                        .TaxType4(bc.getTaxType4())
                        .CurrentSellRate(bc.getCurrentSellRate())
                        .SellRateCurrency(bc.getSellRateCurrency())
                        .LocalTax(bc.getLocalTax())
                        .DebtorCode(bc.getDebtor() != null ? bc.getDebtor().getOrgCode() : null)
                        .CreditorCode(bc.getCreditor() != null ? bc.getCreditor().getOrgCode() : null)
                        .DebitorAddressCode(bc.getDebtor() != null ? bc.getDebtor().getAddressCode() : null)
                        .CreditorAddressCode(bc.getCreditor() != null ? bc.getCreditor().getAddressCode() : null)
                        .PerMeasurementBasis(bc.getMeasurementBasis())
                        .MeasurementsUnit(bc.getMeasurementUnit())
                        .TotalUnitsCount(bc.getTotalUnitCount())
                        .internalRemarks(bc.getInternalRemarks())
                        .externalRemarks(bc.getExternalRemarks())
                        .build()).collect(Collectors.toList());
    }

    private static List<UUID> createContainersGuid(BookingCharges bc) {
        if (bc.getContainersList() == null)
            return new ArrayList<>();
        return bc.getContainersList().stream().filter(Objects::nonNull)
                .map(BaseEntity::getGuid).toList();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.LooseCargo> createLooseCarges(List<Packing> packingList) {
        if (packingList == null)
            return null;
        return packingList.stream().filter(Objects::nonNull)
                .map(packing -> CreateBookingModuleInV1.BookingEntity.LooseCargo.builder()
                        .ReferenceGuid(packing.getGuid())
                        .Packs(packing.getPacks() == null ? 0 : Long.valueOf(packing.getPacks()))
                        .PacksUnit(packing.getPacksType())
                        .Length(packing.getLength())
                        .Height(packing.getHeight())
                        .Width(packing.getWidth())
                        .Du(packing.getLengthUnit())
                        .Weight(packing.getWeight())
                        .WeightUnit(packing.getWeightUnit())
                        .Volume(packing.getVolume())
                        .VolumeUnit(packing.getVolumeUnit())
                        .Chargeable(packing.getChargeable())
                        .ChargeableUnit(packing.getChargeableUnit())
                        .GoodsDescription(packing.getGoodsDescription())
                        .CommodityCode(packing.getCommodity())
                        .CommodityGroup(packing.getCommodityGroup())
                        .HazardousCheckBox(packing.getHazardous())
                        .HsCode(packing.getHSCode())
                        .build()).toList();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.Routing> createRoutingList(List<Routings> routingList) {
        if (routingList == null)
            return null;
        return routingList.stream().filter(Objects::nonNull).map(routings ->
                CreateBookingModuleInV1.BookingEntity.Routing.builder()
                        .ReferenceGuid(routings.getGuid())
                        .Leg(routings.getLeg())
                        .Mode(routings.getMode())
                        .PolCode(routings.getPol())
                        .PodCode(routings.getPod())
                        .build()
        ).toList();
    }

    private static List<CreateBookingModuleInV1.BookingEntity.QuoteContainer> createContainers(List<Containers> containersList) {
        if (containersList == null)
            return null;
        return containersList.stream().filter(Objects::nonNull).map(container ->
                CreateBookingModuleInV1.BookingEntity.QuoteContainer.builder()
                        .ContainerTypeCode(container.getContainerCode())
                        .Count(container.getContainerCount())
                        .CommodityCode(container.getCommodityCode())
                        .CommodityGroup(container.getCommodityGroup())
                        .Weight(container.getGrossWeight())
                        .WeightUnit(container.getGrossWeightUnit())
                        .ReferenceGuid(container.getGuid())
                        .build()
        ).toList();
    }

    public CheckCreditLimitFromV1Response validateCreditLimit(Parties client, String restrictedItem, UUID shipmentGuid, Boolean taskCreation) {
        try {
            CheckCreditLimitFromV1Response creditLimitResponse = CheckCreditLimitFromV1Response.builder().isValid(true).build();
            if(Boolean.FALSE.equals(commonUtils.getCurrentTenantSettings().getEnableCreditLimitManagement())){
                return creditLimitResponse;
            }
            Integer clientId = null;
            Integer clientAddressId = null;
            if(client.getOrgData().containsKey("Id"))
                clientId = (Integer) client.getOrgData().get("Id");
            if(client.getAddressData().containsKey("Id"))
                clientAddressId = (Integer)client.getAddressData().get("Id");
            CreditLimitValidateResponse response = v1Service.checkCreditLimit(CreditLimitValidateRequest.builder()
                    .restrictedItem(restrictedItem)
                    .clientId(clientId)
                    .clientAddressId(clientAddressId)
                    .shipmentGuid(StringUtility.convertToString(shipmentGuid))
                    .taskCreation(taskCreation)
                    .build());
            if (!Boolean.TRUE.equals(response.getIsValid())){
                if(response.getTaskRequiredMessage() != null){
                    creditLimitResponse.setIsValid(response.getIsValid());
                    creditLimitResponse.setTaskRequiredMessage(response.getTaskRequiredMessage());
                    creditLimitResponse.setMessage(response.getMessage());
                    return creditLimitResponse;
                }
                log.error(response.getMessage() + " " + response.getError());
                throw new ValidationException(response.getMessage());
            }
            return creditLimitResponse;
        } catch (V1ServiceException ex) {
            log.error(ShipmentConstants.CHECK_CREDIT_LIMIT_FAILED + ex.getMessage());
            throw new ValidationException(ex.getMessage());
        } catch (Exception ex) {
            log.error(ShipmentConstants.CHECK_CREDIT_LIMIT_FAILED + ex.getMessage());
            throw new ValidationException(ShipmentConstants.CHECK_CREDIT_LIMIT_FAILED + ex.getMessage());
        }
    }

    public ResponseEntity<IRunnerResponse> fetchEmailIdsForShipment(ShipmentDetails shipmentDetail) {
        Set<String> emailSet = new HashSet<>();
        OrgAddressResponse response = fetchOrgInfoFromV1(Arrays.asList(shipmentDetail.getClient(), shipmentDetail.getConsignee(), shipmentDetail.getConsigner()));
        setEmails(shipmentDetail.getClient(), emailSet, response.getOrganizations(), response.getAddresses());
        if (Objects.equals(shipmentDetail.getDirection(), Constants.DIRECTION_IMP))
            setEmails(shipmentDetail.getConsignee(), emailSet, response.getOrganizations(), response.getAddresses());
        else
            setEmails(shipmentDetail.getConsigner(), emailSet, response.getOrganizations(), response.getAddresses());
        return ResponseHelper.buildSuccessResponse(String.join(";", emailSet.stream().filter(StringUtility::isNotEmpty).toList()));
    }

    public ResponseEntity<IRunnerResponse> fetchEmailIdsForConsolidation(ConsolidationDetails consolidationDetail) {
        Set<String> emailSet = new HashSet<>();
        OrgAddressResponse response = fetchOrgInfoFromV1(Arrays.asList(consolidationDetail.getSendingAgent(), consolidationDetail.getReceivingAgent()));
        if (Objects.equals(consolidationDetail.getShipmentType(), Constants.DIRECTION_IMP))
            setEmails(consolidationDetail.getReceivingAgent(), emailSet, response.getOrganizations(), response.getAddresses());
        else
            setEmails(consolidationDetail.getSendingAgent(), emailSet, response.getOrganizations(), response.getAddresses());
        return ResponseHelper.buildSuccessResponse(String.join(";", emailSet.stream().filter(StringUtility::isNotEmpty).toList()));
    }

    private void setEmails(Parties party, Set<String> emailSet, Map<String, Map<String, Object>> organizations, Map<String, Map<String, Object>> addresses) {
        if (Objects.isNull(party) || Objects.isNull(party.getOrgCode()))
            return;

        if (organizations.containsKey(party.getOrgCode()) && organizations.get(party.getOrgCode()).containsKey(PartiesConstants.EMAIL))
            emailSet.add(StringUtility.convertToString(organizations.get(party.getOrgCode()).get(PartiesConstants.EMAIL)));

        String key = party.getOrgCode() + "#" + party.getAddressCode();
        if (addresses.get(key) != null && addresses.get(key).containsKey(PartiesConstants.EMAIL) )
            emailSet.add(StringUtility.convertToString(addresses.get(key).get(PartiesConstants.EMAIL)));

    }

    private AddressTranslationRequest.OrgAddressCode createV1OrgRequest(Parties parties) {
        if (Objects.isNull(parties) || Objects.isNull(parties.getOrgCode()) || Objects.isNull(parties.getAddressCode()))
            return null;
        return AddressTranslationRequest.OrgAddressCode.builder().OrgCode(parties.getOrgCode()).AddressCode(parties.getAddressCode()).build();
    }

    public OrgAddressResponse fetchOrgInfoFromV1(List<Parties> parties) {
        var orgRequest = new ArrayList<AddressTranslationRequest.OrgAddressCode>();
        parties.forEach(p -> orgRequest.add(createV1OrgRequest(p)));
        return v1Service.fetchOrgAddresses(AddressTranslationRequest.builder().OrgAddressCodeList(orgRequest.stream().filter(Objects::nonNull).toList()).build());
    }

    private String getLastLoadJson(List<Containers> containersList) {
        if (Objects.isNull(containersList))
            return null;
        var list = new ArrayList<CreateBookingModuleInV1.BookingEntity.LastTransactionLoadDetails>();
        containersList.forEach(c -> {
            var currentLoad = new CreateBookingModuleInV1.BookingEntity.LastTransactionLoadDetails();
            currentLoad.setLoadKey(generateLoadKeyForContainer(c));
            currentLoad.setLoadQuantity(Objects.isNull(c.getContainerCount()) ? 1 : c.getContainerCount().intValue());
            list.add(currentLoad);
        });
        return jsonHelper.convertToJson(list);
    }

    private String generateLoadKeyForContainer(Containers container) {
        return StringUtility.convertToString(container.getGuid()) + "#"
                + (StringUtility.isNotEmpty(container.getContainerCode()) ? container.getContainerCode() : NPMConstants.ANY) + "#"
                + (StringUtility.isNotEmpty(container.getCommodityGroup()) ? container.getCommodityGroup() : NPMConstants.FAK);
    }

    public Map<Integer, Object> getTenantDetails(List<Integer> tenantIds) {
        if (tenantIds.isEmpty())
            return new HashMap<>();

        try {
            var v1Response = v1Service.getTenantDetails(TenantDetailsByListRequest.builder().tenantIds(tenantIds).take(100).build());
            return v1Response.getEntities()
                    .stream()
                    .collect(Collectors.groupingBy(
                            TenantDetailsByListResponse.TenantDetails::getTenantId,
                            Collectors.collectingAndThen(
                                    Collectors.toList(),
                                    list -> list.get(0).getTenant()
                            )));
        }
        catch (Exception ex) {
            log.error(ex.getMessage());
            return new HashMap<>();
        }
    }

    public Map<Integer, V1TenantSettingsResponse> getTenantSettingsMap(List<Integer> tenantIds) {
        if (tenantIds.isEmpty())
            return new HashMap<>();

        try {
            var v1Response = v1Service.getTenantDetails(TenantDetailsByListRequest.builder().tenantIds(tenantIds).take(100).build());
            return v1Response.getEntities()
                .stream()
                .collect(Collectors.groupingBy(
                    TenantDetailsByListResponse.TenantDetails::getTenantId,
                    Collectors.collectingAndThen(
                        Collectors.toList(),
                        list ->  modelMapper.map(list.get(0).getTenantSettings(), V1TenantSettingsResponse.class)
                    )));
        }
        catch (Exception ex) {
            log.error(ex.getMessage());
            return new HashMap<>();
        }
    }


    public Map<Integer, Set<Integer>> fetchCoLoadInfo(List<Integer> sendToBranch, String field) {
        List<Object> criteria = new ArrayList<>(List.of(List.of(field), "In", List.of(sendToBranch)));
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(100).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.getCoLoadingStations(commonV1ListRequest);
        var coloadInfoList = Optional.ofNullable(jsonHelper.convertValueToList(v1DataResponse.getEntities(), CoLoadingMAWBDetailsResponse.class))
                .orElse(Collections.emptyList());

        return coloadInfoList.stream()
                .collect(Collectors.groupingBy(
                        CoLoadingMAWBDetailsResponse::getParentTenantId,
                        Collectors.collectingAndThen(
                                Collectors.toList(),
                                list -> list.stream().map(CoLoadingMAWBDetailsResponse::getChildTenantId).collect(Collectors.toSet())
                        )));
    }

    public PartiesRequest getPartiesRequestFromOrgIdAndAddressId(Long orgId, Long addressId) {
        try {
            PartiesRequest request = new PartiesRequest();
            CommonV1ListRequest orgRequest = createCriteriaForTwoFields("Id", orgId, "ActiveClient", Boolean.TRUE);
            V1DataResponse v1OrgResponse = v1Service.fetchOrganization(orgRequest);
            List<EntityTransferOrganizations> organizationsList = jsonHelper.convertValueToList(v1OrgResponse.getEntities(), EntityTransferOrganizations.class);
            if (CommonUtils.listIsNullOrEmpty(organizationsList)) {
                throw new DataRetrievalFailureException("No organization exist in Runner V1 with OrgId: " + orgId);
            }
            Map<String, Object> organizationMap = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(organizationsList.get(0)));
            if(organizationMap.containsKey("Id"))
                request.setOrgId(String.valueOf(organizationMap.get("Id")));
            if(organizationMap.containsKey("OrganizationCode"))
                request.setOrgCode(String.valueOf(organizationMap.get("OrganizationCode")));
            request.setOrgData(organizationMap);
            if(organizationMap.containsKey("TenantId"))
                request.setTenantId((Integer) organizationMap.get("TenantId"));

            CommonV1ListRequest addressRequest = createCriteriaForTwoFields("Id", addressId, "Active", Boolean.TRUE);
            V1DataResponse v1AddressResponse = v1Service.addressList(addressRequest);
            List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(v1AddressResponse.getEntities(), EntityTransferAddress.class);
            if (CommonUtils.listIsNullOrEmpty(addressList)) {
                throw new DataRetrievalFailureException("No Address exist in Runner V1 with AddressId: " + addressId);
            }
            Map<String, Object> addressMap = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(addressList.get(0)));
            if(addressMap.containsKey("Id"))
                request.setAddressId(String.valueOf(addressMap.get("Id")));
            if(addressMap.containsKey("AddressShortCode"))
                request.setAddressCode(String.valueOf(addressMap.get("AddressShortCode")));
            request.setAddressData(addressMap);
            return request;
        }
        catch (Exception ex) {
            throw new DataRetrievalFailureException(ex.getMessage());
        }
    }

    private CommonV1ListRequest createCriteriaForTwoFields(String field1, Object value1, String field2, Object value2) {
        List<Object> field1List = new ArrayList<>(List.of(field1));
        List<Object> criteria1 = new ArrayList<>(List.of(field1List, "=", value1));

        List<Object> field2List = new ArrayList<>(List.of(field2));
        List<Object> criteria2 = new ArrayList<>(List.of(field2List, "=", value2));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(criteria1, "and", criteria2)).build();
    }

    public PartiesResponse getDefaultAgentOrg(TenantModel tenantModel) {
        if(tenantModel == null) {
            tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        }
        PartiesResponse partiesResponse = null;
        if(tenantModel.getDefaultOrgId() != null && tenantModel.getDefaultAddressId() != null) {
            PartiesRequest partiesRequest = getPartiesRequestFromOrgIdAndAddressId(tenantModel.getDefaultOrgId(), tenantModel.getDefaultAddressId());
            partiesResponse = jsonHelper.convertValue(partiesRequest, PartiesResponse.class);
        }
        return partiesResponse;
    }

    public Parties getDefaultAgentOrgParty(TenantModel tenantModel) {
        if(tenantModel == null) {
            tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        }
        Parties parties = null;
        if(tenantModel.getDefaultOrgId() != null && tenantModel.getDefaultAddressId() != null) {
            PartiesRequest partiesRequest = getPartiesRequestFromOrgIdAndAddressId(tenantModel.getDefaultOrgId(), tenantModel.getDefaultAddressId());
            parties = jsonHelper.convertValue(partiesRequest, Parties.class);
        }
        return parties;
    }

    public List<UsersDto> getUsersWithGivenPermission(List<String> permissionKeys, Integer tenantId) {
        UserWithPermissionRequestV1 request = new UserWithPermissionRequestV1();
        request.setUserTenantId(tenantId);
        request.setPermissionKeys(permissionKeys);

        return v1Service.getUsersWithGivenPermissions(request);
    }
}
