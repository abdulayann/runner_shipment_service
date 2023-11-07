package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.SBUtilsImpl;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class ShipmentDao implements IShipmentDao {
    @Autowired
    private IShipmentRepository shipmentRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private SBUtilsImpl sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private IMawbStocksDao mawbStocksDao;

    @Autowired
    private IMawbStocksLinkDao mawbStocksLinkDao;

    @Autowired
    private IV1Service v1Service;

    @Value("${shipmentsKafka.queue}")
    private String senderQueue;

    @Autowired
    private KafkaProducer producer;

    @Override
    public ShipmentDetails save(ShipmentDetails shipmentDetails, boolean fromV1Sync) {
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
        }
        else {
            if(shipmentDetails.getConsolidationList() == null)
                shipmentDetails.setConsolidationList(new ArrayList<>());
            if(shipmentDetails.getContainersList() == null)
                shipmentDetails.setContainersList(new ArrayList<>());
        }
        errors.addAll(applyShipmentValidations(shipmentDetails, oldShipment));
        if (! errors.isEmpty())
            throw new ValidationException(errors.toString());
        if (!fromV1Sync && shipmentDetails.getTransportMode().equals("AIR") && shipmentDetails.getShipmentType().equals("DRT"))
            directShipmentMAWBCheck(shipmentDetails);
        shipmentDetails = shipmentRepository.save(shipmentDetails);
        if(!fromV1Sync && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getJobType() != null && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)) {
            if(shipmentDetails.getMasterBill() != null && !shipmentDetails.getDirection().equals(Constants.IMP)) {
                setMawbStock(shipmentDetails);
            }
        }
//        EventMessage eventMessage = EventMessage.builder().messageType(Constants.SERVICE).entity(Constants.SHIPMENT).request(shipmentDetails).build();
//        sbUtils.sendMessagesToTopic(isbProperties, azureServiceBusTopic.getTopic(), Arrays.asList(new ServiceBusMessage(jsonHelper.convertToJson(eventMessage))));
        try {
            producer.produceToKafka(jsonHelper.convertToJson(shipmentDetails), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e)
        {
            log.error("Error pushing to kafka");
        }
        return shipmentDetails;
    }

    public List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments)
    {
        List<ShipmentDetails> res = new ArrayList<>();
        for(ShipmentDetails req : shipments){
            req = save(req, false);
            res.add(req);
        }
        return res;
    }

    @Override
    public ShipmentDetails update(ShipmentDetails shipmentDetails, boolean fromV1Sync) {
        validateLockStatus(shipmentDetails.getId());
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shipmentDetails) , Constants.SHIPMENT, LifecycleHooks.ON_UPDATE, false);
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
        }
        errors.addAll(applyShipmentValidations(shipmentDetails, oldShipment));
        if (! errors.isEmpty())
            throw new ValidationException(errors.toString());
        if (!fromV1Sync && shipmentDetails.getTransportMode().equals("AIR") && shipmentDetails.getShipmentType().equals("DRT"))
            directShipmentMAWBCheck(shipmentDetails);
        shipmentDetails = shipmentRepository.save(shipmentDetails);
        if(!fromV1Sync && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getJobType() != null && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)) {
            if(shipmentDetails.getMasterBill() != null && !shipmentDetails.getDirection().equals(Constants.IMP)) {
                setMawbStock(shipmentDetails);
            }
        }
        try {
            producer.produceToKafka(jsonHelper.convertToJson(shipmentDetails), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e)
        {
            log.error("Error pushing to kafka");
        }
        return shipmentDetails;
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
        validateLockStatus(shipmentDetails.getId());
        shipmentRepository.delete(shipmentDetails);
    }

    private void validateLockStatus(Long id) throws ValidationException {
        Optional<ShipmentDetails> existingShipment = findById(id);
        if(existingShipment.get().getIsLocked() != null && existingShipment.get().getIsLocked()) {
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
    public void updateDateAndStatus(long id, LocalDateTime date, Integer status){
        Optional<ShipmentDetails> shipmentDetails = shipmentRepository.findById(id);
        if(shipmentDetails.isPresent()) {
            ShipmentDetails shipment = shipmentDetails.get();
            if(date != null) {
                shipment.getAdditionalDetails().setDateOfIssue(date);
            }
            if(status != null) {
                shipment.setStatus(status);
            }
            shipmentRepository.save(shipment);
        }
    }

    public Set<String> applyShipmentValidations(ShipmentDetails request, ShipmentDetails oldEntity) {
        Set<String> errors = new LinkedHashSet<>();

        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
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
        if (!IsStringNullOrEmpty(request.getMasterBill()) && (oldEntity == null || IsStringNullOrEmpty(oldEntity.getMasterBill()) || !oldEntity.getMasterBill().equals(request.getMasterBill()))) {
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
        if (shipmentSettingsDetails.getRestrictedLocationsEnabled() && request.getCarrierDetails() != null) {
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
            if(res.getStatus() != "Consumed") {
                res.setEntityId(shipmentDetails.getId());
                res.setEntityType(Constants.SHIPMENT);
                res.setStatus("Consumed");
                mawbStocksLinkDao.save(res);
                setAvaliableCount(res.getParentId());
            }
        }
    }

    private void setAvaliableCount(Long parentId) {
        Optional<MawbStocks> mawbStocks = mawbStocksDao.findById(parentId);
        if(!mawbStocks.isEmpty()) {
            MawbStocks res = mawbStocks.get();
            res.setAvailableCount(String.valueOf(Integer.parseInt(res.getAvailableCount()) - 1));
            res.setNextMawbNumber(assignNextMawbNumber(parentId));
            mawbStocksDao.save(res);
        }
    }

    private String assignNextMawbNumber(Long parentId) {
        ListCommonRequest listCommonRequest;
        listCommonRequest = CommonUtils.andCriteria("parentId", parentId, "=", null);
        CommonUtils.andCriteria("status", "Unused", "=", listCommonRequest);
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

    private void directShipmentMAWBCheck(ShipmentDetails shipmentRequest) {
        if (StringUtility.isEmpty(shipmentRequest.getMasterBill())) {
            return;
        }

        if (!isMAWBNumberValid(shipmentRequest.getMasterBill()))
            throw new ValidationException("Please enter a valid MAWB number.");

        String mawbAirlineCode = shipmentRequest.getMasterBill().substring(0, 3);

        V1DataResponse v1DataResponse = fetchCarrierDetailsFromV1(mawbAirlineCode);
        List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);
        if (carrierDetails == null || carrierDetails.size()==0)
            throw new ValidationException("Airline for the entered MAWB Number doesn't exist in Carrier Master");


        CarrierResponse correspondingCarrier = carrierDetails.get(0); //carrierDetails.getContent().get(0);

        Boolean isMAWBNumberExist = false;
        Boolean isCarrierExist = false;
        if (shipmentRequest.getCarrierDetails() != null)
            isCarrierExist = true;

        if (isCarrierExist && !shipmentRequest.getCarrierDetails().getShippingLine().equals(correspondingCarrier.getItemValue()))
            throw new ValidationException("MAWB Number prefix is not matching with entered Flight Carrier");

        ListCommonRequest listMawbRequest = constructListCommonRequest("mawbNumber", shipmentRequest.getMasterBill(), "=");
        Pair<Specification<MawbStocksLink>, Pageable> mawbStocksLinkPair = fetchData(listMawbRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinkPage = mawbStocksLinkDao.findAll(mawbStocksLinkPair.getLeft(), mawbStocksLinkPair.getRight());

        MawbStocksLink mawbStocksLink = null;

        if (mawbStocksLinkPage.getContent() != null && mawbStocksLinkPage.getTotalElements() > 0) {
            isMAWBNumberExist = true;
            mawbStocksLink = mawbStocksLinkPage.getContent().get(0);
        }

        if (!isCarrierExist)
            shipmentRequest.setCarrierDetails(jsonHelper.convertValue(correspondingCarrier, CarrierDetails.class));

        if (shipmentRequest.getDirection() == "IMP") {
            return;
        }

        if (isMAWBNumberExist) {
            if (mawbStocksLink.getStatus() == "Consumed" && mawbStocksLink.getEntityId() != shipmentRequest.getId()) // If MasterBill number is already Consumed.
                throw new ValidationException("The MAWB number entered is already consumed. Please enter another MAWB number.");
        } else {
            createNewMAWBEntry(shipmentRequest);
        }
    }

    private void createNewMAWBEntry(ShipmentDetails shipmentRequest) {
        MawbStocks mawbStocks = new MawbStocks();
        mawbStocks.setAirLinePrefix(shipmentRequest.getCarrierDetails().getShippingLine());
        mawbStocks.setCount("1");
        mawbStocks.setStartNumber(Long.valueOf(shipmentRequest.getMasterBill().substring(4, 10)));
        mawbStocks.setFrom(shipmentRequest.getMasterBill());
        mawbStocks.setTo(shipmentRequest.getMasterBill());
        mawbStocks.setMawbNumber(shipmentRequest.getMasterBill());
        mawbStocks.setStatus("Unused");
        // if(shipmentRequest.getBorrowedFrom()!=null) mawbStocks.setBorrowedFrom(Long.valueOf(shipmentRequest.getBorrowedFrom())); TODO fetch from v1
        mawbStocks.setHomePort(shipmentRequest.getCarrierDetails().getOriginPort());
        mawbStocks = mawbStocksDao.save(mawbStocks);

        if (mawbStocks.getId() != null) {
            var entryForMawbStocksLinkRow = new MawbStocksLink();
            entryForMawbStocksLinkRow.setParentId(mawbStocks.getId());
            entryForMawbStocksLinkRow.setSeqNumber(shipmentRequest.getMasterBill().substring(4, 10));
            entryForMawbStocksLinkRow.setMawbNumber(shipmentRequest.getMasterBill());
            entryForMawbStocksLinkRow.setStatus("Unused");
            entryForMawbStocksLinkRow = mawbStocksLinkDao.save(entryForMawbStocksLinkRow);
        }
    }

    private Boolean isMAWBNumberValid(String masterBill) {
        Boolean MAWBNumberValidity = true;
        if (masterBill.length() == 12) {
            String mawbSeqNum = masterBill.substring(4, 11);
            String checkDigit = masterBill.substring(11, 12);
            Long imawbSeqNum = 0L;
            Long icheckDigit = 0L;
            if (areAllCharactersDigits(masterBill, 4, 12)) { // masterBill.substring(4, 12).matches("\\d+")
                imawbSeqNum = Long.valueOf(mawbSeqNum);
                icheckDigit = Long.valueOf(checkDigit);
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

    private V1DataResponse fetchCarrierDetailsFromV1(String mawbAirlineCode) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        criteria.addAll(List.of(List.of("AirlineCode"), "=", mawbAirlineCode));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);
        return response;
    }

}
