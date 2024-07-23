package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.validation.ConstraintViolationException;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;

@Repository
@Slf4j
public class ShipmentDao implements IShipmentDao {
    public static final String CONSUMED = "Consumed";
    public static final String UNUSED = "Unused";
    @Autowired
    private IShipmentRepository shipmentRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ISBUtils sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;


    @Autowired
    private IMawbStocksDao mawbStocksDao;

    @Autowired
    private IMawbStocksLinkDao mawbStocksLinkDao;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private CommonUtils commonUtils;

    @Override
    public ShipmentDetails save(ShipmentDetails shipmentDetails, boolean fromV1Sync) throws RunnerException {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shipmentDetails) , Constants.SHIPMENT, LifecycleHooks.ON_CREATE, false);
        ShipmentDetails oldShipment = null;
        if(shipmentDetails.getId() != null){
            long id = shipmentDetails.getId();
            Optional<ShipmentDetails> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Container is null for Id {}", shipmentDetails.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(shipmentDetails.getContainersList() == null) {
                shipmentDetails.setContainersList(oldEntity.get().getContainersList());
            }
            if(shipmentDetails.getConsolidationList() == null) {
                shipmentDetails.setConsolidationList(oldEntity.get().getConsolidationList());
            }
            oldShipment = oldEntity.get();
            shipmentDetails.setCreatedBy(oldShipment.getCreatedBy());
        }
        else {
            if(shipmentDetails.getConsolidationList() == null)
                shipmentDetails.setConsolidationList(new ArrayList<>());
            if(shipmentDetails.getContainersList() == null)
                shipmentDetails.setContainersList(new ArrayList<>());
        }
        try {
            onSave(shipmentDetails, errors, oldShipment, fromV1Sync);
        } catch (Exception e){
            String errorMessage = e.getMessage();
            if(e.getClass().equals(ConstraintViolationException.class))
                errorMessage = getConstrainViolationErrorMessage(e);
            throw new RunnerException(errorMessage);
        }
        return shipmentDetails;
    }

    public List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments) throws RunnerException {
        List<ShipmentDetails> res = new ArrayList<>();
        for(ShipmentDetails req : shipments){
            req = save(req, false);
            res.add(req);
        }
        return res;
    }

    @Override
    public ShipmentDetails update(ShipmentDetails shipmentDetails, boolean fromV1Sync) {
        validateLockStatus(shipmentDetails);
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shipmentDetails) , Constants.SHIPMENT, LifecycleHooks.ON_CREATE, false);
        ShipmentDetails oldShipment = null;
        if(shipmentDetails.getId() != null){
            long id = shipmentDetails.getId();
            Optional<ShipmentDetails> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment is null for Id {}", shipmentDetails.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(shipmentDetails.getContainersList() == null) {
                shipmentDetails.setContainersList(oldEntity.get().getContainersList());
            }
            if(shipmentDetails.getConsolidationList() == null) {
                shipmentDetails.setConsolidationList(oldEntity.get().getConsolidationList());
            }
            oldShipment = oldEntity.get();
            shipmentDetails.setCreatedBy(oldShipment.getCreatedBy());
        }
        onSave(shipmentDetails, errors, oldShipment, fromV1Sync);
        return shipmentDetails;
    }

    private void onSave(ShipmentDetails shipmentDetails, Set<String> errors, ShipmentDetails oldShipment, boolean fromV1Sync) {
        if (!StringUtility.isEmpty(shipmentDetails.getHouseBill()) && (oldShipment != null && !Objects.equals(oldShipment.getStatus(), shipmentDetails.getStatus())) &&
                Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
            ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();
            if (tenantSettings != null) {
                String suffix = tenantSettings.getCancelledBLSuffix();
                if (suffix != null) {
                    String newHouseBill = shipmentDetails.getHouseBill() + suffix;
                    shipmentDetails.setHouseBill(newHouseBill);
                }
            }
        }
        errors.addAll(applyShipmentValidations(shipmentDetails, fromV1Sync));
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (shipmentDetails.getTransportMode() != null && shipmentDetails.getCarrierDetails() != null) {
            LocalDateTime eta = shipmentDetails.getCarrierDetails().getEta();
            LocalDateTime etd = shipmentDetails.getCarrierDetails().getEtd();
            if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                //for air shipment, ETA can be less than ETD
                if (eta != null && etd != null && eta.isBefore(etd)) {
                    Duration duration = Duration.between(eta, etd);
                    if (Math.abs(duration.toHours()) > 24) {
                        throw new ValidationException("Difference between ETA and ETD should not be more than 24 hours");
                    }
                }
            } else {
                //for other transport modes other than AIR, ETA cannot be less than ETD
                if (eta != null && etd != null && eta.isBefore(etd)) {
                    throw new ValidationException("ETA should not be less than ETD");
                }
            }
        }
        if (!fromV1Sync && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipmentDetails.getJobType() != null && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT))
            directShipmentMAWBCheck(shipmentDetails, oldShipment != null ? oldShipment.getMasterBill() : null);

        validateIataCode(shipmentDetails);

        shipmentDetails = shipmentRepository.save(shipmentDetails);
        if (!fromV1Sync && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getJobType() != null && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)) {
            if (shipmentDetails.getMasterBill() != null && !shipmentDetails.getDirection().equals(Constants.IMP)) {
                setMawbStock(shipmentDetails);
            }
        }
//        EventMessage eventMessage = EventMessage.builder().messageType(Constants.SERVICE).entity(Constants.SHIPMENT).request(shipmentDetails).build();
//        sbUtils.sendMessagesToTopic(isbProperties, azureServiceBusTopic.getTopic(), Arrays.asList(new ServiceBusMessage(jsonHelper.convertToJson(eventMessage))));
    }

    @Override
    public Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable) {
        return shipmentRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ShipmentDetails> findById(Long id) {
        return shipmentRepository.findById(id);
    }

    @Override
    public void delete(ShipmentDetails shipmentDetails) {
        validateLockStatus(shipmentDetails);
        shipmentRepository.delete(shipmentDetails);
    }

    private void validateLockStatus(ShipmentDetails shipmentDetails) throws ValidationException {
        if(shipmentDetails.getIsLocked() != null && shipmentDetails.getIsLocked()) {
            throw new ValidationException(ShipmentConstants.SHIPMENT_LOCKED);
        }
    }

    @Override
    public Optional<ShipmentDetails> findByGuid(UUID id) {
        return shipmentRepository.findByGuid(id);
    }
    @Override
    public List<ShipmentDetails> findByHouseBill(String Hbl){
        return shipmentRepository.findByHouseBill(Hbl);
    }
    @Override
    public List<ShipmentDetails> findByBookingReference(String ref){
        return shipmentRepository.findByBookingReference(ref);
    }

    @Override
    public Long findMaxId() { return shipmentRepository.findMaxId(); }

    private boolean checkForNonAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(!Constants.TRANSPORT_MODE_AIR.equals(request.getTransportMode()))
            return true;
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getAirDGFlag());
    }

    private boolean checkForDGShipmentAndAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(checkForNonAirDGFlag(request, shipmentSettingsDetails))
            return false;
        return Boolean.TRUE.equals(request.getContainsHazardous());
    }

    private boolean checkForNonDGShipmentAndAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(checkForNonAirDGFlag(request, shipmentSettingsDetails))
            return false;
        return !Boolean.TRUE.equals(request.getContainsHazardous());
    }

    public Set<String> applyShipmentValidations(ShipmentDetails request, boolean fromV1Sync) {
        Set<String> errors = new LinkedHashSet<>();

        if(request.getConsolidationList() != null && request.getConsolidationList().size() > 1) {
            errors.add("Multiple consolidations are attached to the shipment, please verify.");
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        // Non dg Shipments can not have dg packs
        if(checkForNonDGShipmentAndAirDGFlag(request, shipmentSettingsDetails) && request.getPackingList() != null) {
            for (Packing packing: request.getPackingList()) {
                if(Boolean.TRUE.equals(packing.getHazardous())) {
                    errors.add("The shipment contains DG package. Marking the shipment as non DG is not allowed");
                }
            }
        }

        // Non dg user cannot save dg shipment
        if(!fromV1Sync && checkForDGShipmentAndAirDGFlag(request, shipmentSettingsDetails) && !UserContext.isDgUser())
            errors.add("You don't have permission to update DG Shipment");
        
        // Routings leg no can not be repeated
        if (request.getRoutingsList() != null && request.getRoutingsList().size() > 0) {
            HashSet<Long> hashSet = new HashSet<>();
            for (Routings routingsRequest : request.getRoutingsList()) {
                if (routingsRequest.getLeg() != null) {
                    if (hashSet.contains(routingsRequest.getLeg())) {
                        errors.add("Leg No in routings cannot be same for two different legs");
                        break;
                    } else
                        hashSet.add(routingsRequest.getLeg());
                }
            }
        }

        // Origin and Destinations are mandatory
        if(request.getCarrierDetails() == null || IsStringNullOrEmpty(request.getCarrierDetails().getOrigin()) || IsStringNullOrEmpty(request.getCarrierDetails().getDestination()))
            errors.add("Origin and Destination fields are mandatory.");

        // POL and POD are mandatory for Sea and Air
        if( ( Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA) || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) ) &&
                (request.getCarrierDetails() == null || IsStringNullOrEmpty(request.getCarrierDetails().getOriginPort()) || IsStringNullOrEmpty(request.getCarrierDetails().getDestinationPort()) ))
            errors.add("POL and POD fields are mandatory.");

        // Container Number can not be repeated
        if (request.getContainersList() != null && request.getContainersList().size() > 0) {
            HashSet<String> hashSet = new HashSet<>();
            for (Containers containers : request.getContainersList()) {
                if (!IsStringNullOrEmpty(containers.getContainerNumber())) {
                    if (hashSet.contains(containers.getContainerNumber())) {
                        errors.add("Container Number cannot be same for two different containers");
                        break;
                    } else
                        hashSet.add(containers.getContainerNumber());
                }
            }
        }

        // Duplicate party types not allowed
        if (request.getShipmentAddresses() != null && request.getShipmentAddresses().size() > 0) {
            HashSet<String> partyTypes = new HashSet<>();
            HashSet<String> duplicatePartyTypes = new HashSet<>();
            for (Parties item : request.getShipmentAddresses()) {
                if (partyTypes.contains(item.getType())) {
                    duplicatePartyTypes.add(item.getType());
                } else {
                    partyTypes.add(item.getType());
                }
            }
            if (!duplicatePartyTypes.isEmpty()) {
                String types = String.join(", ", duplicatePartyTypes);

                String message = (duplicatePartyTypes.size() == 1) ? " is a duplicate Party Type." : " are duplicate Party Types.";
                errors.add(types + message);
            }
        }

        // Shipment must be attached to consolidation with same master bill
        if (!IsStringNullOrEmpty(request.getMasterBill())) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bol", request.getMasterBill(), "=");
            Pair<Specification<ConsolidationDetails>, Pageable> consolidationPage = fetchData(listCommonRequest, ConsolidationDetails.class);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(consolidationPage.getLeft(), consolidationPage.getRight());
            if (!consolidationDetailsPage.isEmpty()) {
                ConsolidationDetails console = consolidationDetailsPage.get().toList().get(0);

                if ((request.getConsolidationList() != null && !request.getConsolidationList().stream().map(ConsolidationDetails::getId).toList().contains(console.getId())) ||
                        (request.getConsolidationList() == null &&
                                (request.getId() == null ||
                                        (console.getShipmentsList() != null && !console.getShipmentsList().stream().map(BaseEntity::getId).toList().contains(request.getId()))))) {
                    String message = "%s %s is linked to consolidation %s. Please attach the shipment to that consolidation.";
                    String masterBillType = request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ? Constants.MAWB : Constants.MBL;
                    errors.add(String.format(message, masterBillType, request.getMasterBill(), console.getConsolidationNumber()));
                }
            }
        }

        // Shipment restricted unlocations validation
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getRestrictedLocationsEnabled()) && request.getCarrierDetails() != null) {
            String unLoc = null;
            if (request.getDirection().equals(Constants.DIRECTION_EXP)) {
                unLoc = request.getCarrierDetails().getOriginPort();
                if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
                    errors.add("Value entered for Loading Port is not allowed or invalid");
                }
            } else if (request.getDirection().equals(Constants.IMP)) {
                unLoc = request.getCarrierDetails().getDestinationPort();
                if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
                    errors.add("Value entered for Discharge Port is not allowed or invalid");
                }
            }
        }

//        // Shipment Lock validation error
//        if(oldEntity != null && oldEntity.getIsLocked()) {
//            List <Object> criteria = Arrays.asList(
//                    Arrays.asList("Username"),
//                    "=",
//                    oldEntity.getLockedBy()
//            );
//            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
//            V1DataResponse v1DataResponse = v1Service.fetchUsersData(commonV1ListRequest);
//            List<UsersDto> usersDtos = jsonHelper.convertValueToList(v1DataResponse.entities, UsersDto.class);
//            String username = "";
//            if(usersDtos != null && usersDtos.size() > 0)
//                username = usersDtos.get(0).Username;
//            errors.add("Shipment is Locked By User " + username + ". Please unlock for any Updation.");
//        }

        // BL# and Reference No can not be repeated
        if(!IsStringNullOrEmpty(request.getHouseBill())) {
            List<ShipmentDetails> shipmentDetails = findByHouseBill(request.getHouseBill());
            if(shipmentDetails != null && shipmentDetails.size() > 0 && (request.getId() == null || shipmentDetails.get(0).getId().longValue() != request.getId().longValue())) {
                if (Objects.equals(request.getStatus(), ShipmentStatus.Cancelled.getValue()))
                    errors.add("Canceled HBL is already available in the application. Please remove/ modify the HBL number to proceed further");
                else
                    errors.add("Shipment with BL# " + request.getHouseBill() + " already exists.");
            }
        }
        if(!IsStringNullOrEmpty(request.getBookingReference())) {
            List<ShipmentDetails> shipmentDetails = findByBookingReference(request.getBookingReference());
            if(!shipmentDetails.isEmpty() && (request.getId() == null || shipmentDetails.get(0).getId().longValue() != request.getId().longValue())) {
                errors.add("Shipment with ReferenceNo " + request.getBookingReference() + " already exists.");
            }
        }

        return errors;
    }

    private void setMawbStock(ShipmentDetails shipmentDetails) {
        List<MawbStocksLink> mawbStocksLinks = mawbStocksLinkDao.findByMawbNumber(shipmentDetails.getMasterBill());
        if(mawbStocksLinks != null && mawbStocksLinks.size() > 0) {
            MawbStocksLink res = mawbStocksLinks.get(0);
            if(!Objects.isNull(res.getStatus()) && !res.getStatus().equals(CONSUMED)) {
                res.setEntityId(shipmentDetails.getId());
                res.setEntityType(Constants.SHIPMENT);
                res.setShipConsNumber(shipmentDetails.getShipmentId());
                res.setStatus(CONSUMED);
                mawbStocksLinkDao.save(res);
                setAvaliableCount(res.getParentId());
            }
        }
    }

    private void setAvaliableCount(Long parentId) {
        Optional<MawbStocks> mawbStocks = mawbStocksDao.findById(parentId);
        if(!mawbStocks.isEmpty()) {
            MawbStocks res = mawbStocks.get();
            res.setAvailableCount(String.valueOf(Integer.parseInt(res.getAvailableCount() == null ? "1" : res.getAvailableCount()) - 1));
            res.setNextMawbNumber(assignNextMawbNumber(parentId));
            mawbStocksDao.save(res);
        }
    }

    private String assignNextMawbNumber(Long parentId) {
        ListCommonRequest listCommonRequest;
        listCommonRequest = CommonUtils.andCriteria("parentId", parentId, "=", null);
        CommonUtils.andCriteria("status", UNUSED, "=", listCommonRequest);
        listCommonRequest.setSortRequest(SortRequest.builder()
                .fieldName("seqNumber")
                .order("DESC")
                .build());
        Pair<Specification<MawbStocksLink>, Pageable> pair = fetchData(listCommonRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinks = mawbStocksLinkDao.findAll(pair.getLeft(), pair.getRight());
        if(!mawbStocksLinks.isEmpty()) {
            return mawbStocksLinks.get().toList().get(0).getMawbNumber();
        }
        return null;
    }

    private void directShipmentMAWBCheck(ShipmentDetails shipmentRequest, String oldMasterBill) {

        if (StringUtility.isEmpty(shipmentRequest.getMasterBill())) {
            if(!shipmentRequest.getDirection().equals("IMP")) {
                mawbStocksLinkDao.deLinkExistingMawbStockLink(oldMasterBill);
            }
            return;
        }
        if (!Objects.equals(shipmentRequest.getMasterBill(), oldMasterBill) && !shipmentRequest.getDirection().equals("IMP")) {
            mawbStocksLinkDao.deLinkExistingMawbStockLink(oldMasterBill);
        }

        if (Boolean.FALSE.equals(isMAWBNumberValid(shipmentRequest.getMasterBill())))
            throw new ValidationException("Please enter a valid MAWB number.");

        CarrierResponse correspondingCarrier = null;
        if(shipmentRequest.getCarrierDetails() == null || StringUtility.isEmpty(shipmentRequest.getCarrierDetails().getShippingLine()) ||
            !Objects.equals(shipmentRequest.getMasterBill(), oldMasterBill)) {
            String mawbAirlineCode = shipmentRequest.getMasterBill().substring(0, 3);

            V1DataResponse v1DataResponse = fetchCarrierDetailsFromV1(mawbAirlineCode, shipmentRequest.getJobType());
            List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);
            if (carrierDetails == null || carrierDetails.isEmpty())
                throw new ValidationException("Airline for the entered MAWB Number doesn't exist in Carrier Master");

            correspondingCarrier = carrierDetails.get(0);

            if(shipmentRequest.getCarrierDetails() == null || StringUtility.isEmpty(shipmentRequest.getCarrierDetails().getShippingLine())) {
                if (shipmentRequest.getCarrierDetails() == null)
                    shipmentRequest.setCarrierDetails(new CarrierDetails());

                shipmentRequest.getCarrierDetails().setShippingLine(correspondingCarrier.getItemValue());
            }
        }

        if (shipmentRequest.getDirection().equals("IMP")) {
            return;
        }

        boolean isMAWBNumberExist = false;

        ListCommonRequest listMawbRequest = constructListCommonRequest("mawbNumber", shipmentRequest.getMasterBill(), "=");
        Pair<Specification<MawbStocksLink>, Pageable> mawbStocksLinkPair = fetchData(listMawbRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinkPage = mawbStocksLinkDao.findAll(mawbStocksLinkPair.getLeft(), mawbStocksLinkPair.getRight());

        MawbStocksLink mawbStocksLink = null;

        if (!mawbStocksLinkPage.isEmpty() && mawbStocksLinkPage.getTotalElements() > 0) {
            isMAWBNumberExist = true;
            mawbStocksLink = mawbStocksLinkPage.getContent().get(0);
        }

        if (isMAWBNumberExist) {
            if (mawbStocksLink.getStatus().equals(CONSUMED) && !Objects.equals(mawbStocksLink.getEntityId(), shipmentRequest.getId())) // If MasterBill number is already Consumed.
                throw new ValidationException("The MAWB number entered is already consumed. Please enter another MAWB number.");
        } else {
            createNewMAWBEntry(shipmentRequest, correspondingCarrier != null ? correspondingCarrier.getItemValue() : shipmentRequest.getCarrierDetails().getShippingLine());
        }
    }

    private void createNewMAWBEntry(ShipmentDetails shipmentRequest, String shippingLine) {
        MawbStocks mawbStocks = new MawbStocks();
        mawbStocks.setAirLinePrefix(shippingLine);
        mawbStocks.setCount("1");
        mawbStocks.setAvailableCount("1");
        mawbStocks.setStartNumber(Long.valueOf(shipmentRequest.getMasterBill().substring(4, 10)));
        mawbStocks.setFrom(shipmentRequest.getMasterBill());
        mawbStocks.setTo(shipmentRequest.getMasterBill());
        mawbStocks.setMawbNumber(shipmentRequest.getMasterBill());
        mawbStocks.setStatus(UNUSED);
        if(shipmentRequest.getAdditionalDetails().getBorrowedFrom()!=null){
             mawbStocks.setBorrowedFrom(shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgCode());
             if(shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgData() != null && shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgData().containsKey("FullName")) {
                 String name = (String) shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgData().get("FullName");
                 mawbStocks.setBorrowedFromFullName(name);
            }
        }
        mawbStocks.setHomePort(shipmentRequest.getCarrierDetails().getOriginPort());
        mawbStocks = mawbStocksDao.save(mawbStocks);

        if (mawbStocks.getId() != null) {
            var entryForMawbStocksLinkRow = new MawbStocksLink();
            entryForMawbStocksLinkRow.setParentId(mawbStocks.getId());
            entryForMawbStocksLinkRow.setSeqNumber(shipmentRequest.getMasterBill().substring(4, 10));
            entryForMawbStocksLinkRow.setMawbNumber(shipmentRequest.getMasterBill());
            entryForMawbStocksLinkRow.setStatus(UNUSED);
            entryForMawbStocksLinkRow = mawbStocksLinkDao.save(entryForMawbStocksLinkRow);
        }
    }

    private Boolean isMAWBNumberValid(String masterBill) {
        Boolean MAWBNumberValidity = true;
        if (masterBill.length() == 12) {
            String mawbSeqNum = masterBill.substring(4, 11);
            String checkDigit = masterBill.substring(11, 12);
            if (areAllCharactersDigits(masterBill, 4, 12)) { // masterBill.substring(4, 12).matches("\\d+")
                Long imawbSeqNum = Long.valueOf(mawbSeqNum);
                Long icheckDigit = Long.valueOf(checkDigit);
                if (imawbSeqNum % 7 != icheckDigit)
                    MAWBNumberValidity = false;
            } else MAWBNumberValidity = false;
        } else MAWBNumberValidity = false;
        return MAWBNumberValidity;
    }

    private boolean areAllCharactersDigits(String input, int startIndex, int endIndex) {
        String substring = input.substring(startIndex, endIndex);
        for (int i = 0; i < substring.length(); i++) {
            if (!Character.isDigit(substring.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private V1DataResponse fetchCarrierDetailsFromV1(String mawbAirlineCode, String type) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        criteria.add(Arrays.asList(List.of("AirlineCode"), "=", mawbAirlineCode));
        criteria.add("and");
        criteria.add(Arrays.asList(List.of("HasAirPort"), "=", 1));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        carrierListObject.setType(type);
        return v1Service.fetchCarrierMasterData(carrierListObject, false);
    }

    private void validateIataCode(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getTransportMode() != null && shipmentDetails.getCarrierDetails() != null && StringUtility.isNotEmpty(shipmentDetails.getCarrierDetails().getShippingLine())
             && shipmentDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
            V1DataResponse v1DataResponse = fetchCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
            List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);
            if (carrierDetails == null || carrierDetails.size()==0 || StringUtility.isEmpty(carrierDetails.get(0).iATACode))
                throw new ValidationException("Please add the IATA code in the Carrier Master for " + shipmentDetails.getCarrierDetails().getShippingLine());
        }
    }

    private V1DataResponse fetchCarrier(String shippingLine) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>(List.of(List.of("ItemValue"), "=", shippingLine));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);
        return response;
    }
    @Transactional
    public void saveJobStatus(Long id, String jobStatus) {
        shipmentRepository.saveJobStatus(id, jobStatus);
    }

    @Transactional
    public void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate) {shipmentRepository.saveCreatedDateAndUser(id, createdBy, createdDate);}

    @Override
    public List<ShipmentDetails> getShipmentNumberFromId(List<Long> shipmentIds) {
        return shipmentRepository.getShipmentNumberFromId(shipmentIds);
    }

    @Override
    @Transactional
    public void saveEntityTransfer(Long id, Boolean entityTransfer){
        shipmentRepository.saveEntityTransfer(id, entityTransfer);
    }

    @Override
    @Transactional
    public List<ShipmentDetails> findShipmentsByGuids(Set<UUID> guids) {
        return shipmentRepository.findShipmentsByGuids(guids);
    }

    @Override
    @Transactional
    public List<ShipmentDetails> findShipmentsBySourceGuids(Set<UUID> sourceGuid) {
        return shipmentRepository.findShipmentsBySourceGuids(sourceGuid);
    }

}
