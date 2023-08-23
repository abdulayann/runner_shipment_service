package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToEntityList;

@Service
@Slf4j
public class CustomerBookingService implements ICustomerBookingService {

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @Autowired
    private IBookingChargesDao bookingChargesDao;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IFileRepoDao fileRepoDao;

    private Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("customerOrgCode", RunnerEntityMapping.builder().tableName("customer").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName("consigner").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("consigneeOrgCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("destination").build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("originPort").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("destinationPort").build()),
            Map.entry("bookingNumber", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(String.class).fieldName("bookingNumber").build()),
            Map.entry("bookingDate", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(LocalDateTime.class).fieldName("bookingDate").build()),
            Map.entry("bookingStatus", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(String.class).fieldName("bookingStatus").build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(String.class).fieldName("createdBy").build())
    );

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {

        CustomerBookingRequest request = (CustomerBookingRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Customer Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        try {
            if(customerBooking.getBookingNumber() == null) {
                customerBooking.setBookingNumber(generateBookingNumber());
            }
            customerBooking = customerBookingDao.save(customerBooking);
            Long bookingId = customerBooking.getId();

            List<PackingRequest> packingRequest = request.getPackingList();
            if (packingRequest != null)
                customerBooking.setPackingList(packingDao.saveEntityFromBooking(convertToEntityList(packingRequest, Packing.class), bookingId));

            List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
            if (fileRepoRequest != null)
                customerBooking.setFileRepoList(fileRepoDao.saveEntityFromOtherEntity(convertToEntityList(fileRepoRequest, FileRepo.class), bookingId, Constants.BOOKING));

            List<RoutingsRequest> routingsRequest = request.getRoutingList();
            if (routingsRequest != null)
                customerBooking.setRoutingList(routingsDao.saveEntityFromBooking(convertToEntityList(routingsRequest, Routings.class), bookingId));

            List<ContainerRequest> containerRequest = request.getContainersList();
            if (containerRequest != null) {
                List<Containers> containers = containerDao.updateEntityFromBooking(convertToEntityList(containerRequest, Containers.class), bookingId);
                customerBooking.setContainersList(containers);
            }

            List<Containers> containers = customerBooking.getContainersList();
            Map<UUID, Containers> containerMap = new HashMap<>();
            if(containers != null && !containers.isEmpty())
            {
                for(Containers container: containers)
                {
                    containerMap.put(container.getGuid(), container);
                }
            }
            List<BookingChargesRequest> bookingChargesRequest = request.getBookingCharges();
            if(bookingChargesRequest != null && !bookingChargesRequest.isEmpty())
            {
                List<BookingCharges> bookingCharges = new ArrayList<>();
                for (BookingChargesRequest bookingChargeRequest : bookingChargesRequest) {
                    List<UUID> containerUUIDs = bookingChargeRequest.getContainersUUID();
                    BookingCharges bookingCharge = jsonHelper.convertValue(bookingChargeRequest, BookingCharges.class);
                    List<Containers> containerList = new ArrayList<>();
                    if(containerUUIDs != null && !containerUUIDs.isEmpty())
                    {
                        for(UUID uuid: containerUUIDs)
                        {
                            Containers container = containerMap.get(uuid);
                            containerList.add(container);
                        }
                    }
                    bookingCharge.setContainersList(containerList);
                    bookingCharge.setBookingId(customerBooking.getId());
                    bookingCharge = bookingChargesDao.save(bookingCharge);
                    bookingCharges.add(bookingCharge);
                }
                customerBooking.setBookingCharges(bookingCharges);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        CustomerBookingRequest request = (CustomerBookingRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Booking update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (request.getId() == null) {
            log.error("Request Id is null for Booking update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<CustomerBooking> oldEntity = customerBookingDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Booking Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        try {
            customerBooking = customerBookingDao.save(customerBooking);
            Long bookingId = customerBooking.getId();

            List<PackingRequest> packingRequest = request.getPackingList();
            if (packingRequest != null)
                customerBooking.setPackingList(packingDao.updateEntityFromBooking(convertToEntityList(packingRequest, Packing.class), bookingId));

            List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
            if (fileRepoRequest != null)
                customerBooking.setFileRepoList(fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequest, FileRepo.class), bookingId, Constants.BOOKING));

            List<RoutingsRequest> routingsRequest = request.getRoutingList();
            if (routingsRequest != null)
                customerBooking.setRoutingList(routingsDao.updateEntityFromBooking(convertToEntityList(routingsRequest, Routings.class), bookingId));

            List<ContainerRequest> containerRequest = request.getContainersList();
            if (containerRequest != null) {
                List<Containers> containers = containerDao.updateEntityFromBooking(convertToEntityList(containerRequest, Containers.class), bookingId);
                customerBooking.setContainersList(containers);
            }

            List<Containers> containers = customerBooking.getContainersList();
            Map<UUID, Containers> containerMap = new HashMap<>();
            if(containers != null && !containers.isEmpty())
            {
                for(Containers container: containers)
                {
                    containerMap.put(container.getGuid(), container);
                }
            }
            List<BookingChargesRequest> bookingChargesRequest = request.getBookingCharges();
            if(bookingChargesRequest != null && !bookingChargesRequest.isEmpty())
            {
                List<BookingCharges> bookingCharges = new ArrayList<>();
                for (BookingChargesRequest bookingChargeRequest : bookingChargesRequest) {
                    List<UUID> containerUUIDs = bookingChargeRequest.getContainersUUID();
                    BookingCharges bookingCharge = jsonHelper.convertValue(bookingChargeRequest, BookingCharges.class);
                    List<Containers> containerList = new ArrayList<>();
                    if(containerUUIDs != null && !containerUUIDs.isEmpty())
                    {
                        for(UUID uuid: containerUUIDs)
                        {
                            Containers container = containerMap.get(uuid);
                            containerList.add(container);
                        }
                    }
                    bookingCharge.setContainersList(containerList);
                    bookingCharge.setBookingId(customerBooking.getId());
                    bookingCharges.add(bookingCharge);
                }
                bookingCharges = bookingChargesDao.updateEntityFromBooking(bookingCharges, bookingId);
                customerBooking.setBookingCharges(bookingCharges);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Booking list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<CustomerBooking>, Pageable> tuple = fetchData(request, CustomerBooking.class, tableNames);
            Page<CustomerBooking> customerBookingPage = customerBookingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Booking list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(customerBookingPage.getContent()),
                    customerBookingPage.getTotalPages(),
                    customerBookingPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Booking async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<CustomerBooking>, Pageable> tuple = fetchData(request, CustomerBooking.class, tableNames);
            Page<CustomerBooking> customerBookingPage = customerBookingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Booking async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(customerBookingPage.getContent()),
                    customerBookingPage.getTotalPages(),
                    customerBookingPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Booking delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Booking delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<CustomerBooking> customerBooking = customerBookingDao.findById(id);
            if (!customerBooking.isPresent()) {
                log.debug("Booking Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            customerBookingDao.delete(customerBooking.get());
            log.info("Deleted Booking details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<CustomerBooking> customerBooking = customerBookingDao.findById(id);
            if (!customerBooking.isPresent()) {
                log.debug("Booking Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Booking details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking.get(), CustomerBookingResponse.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> platformCreateBooking(CommonRequestModel commonRequestModel) {
        PlatformToRunnerCustomerBookingRequest request = (PlatformToRunnerCustomerBookingRequest) commonRequestModel.getData();
        return null;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<CustomerBooking> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(customerBooking -> {
            CustomerBookingResponse response = modelMapper.map(customerBooking, CustomerBookingResponse.class);
            responseList.add(response);
        });
        return responseList;
    }

    private String generateBookingNumber()
    {
        return "SROF" + "-" + getRandomNumberString(6) + "-" + getRandomNumberString(5);
    }

    public static String getRandomNumberString(int digit) {
        Random rnd = new Random();
        int number = 0;
        if(digit == 6)
            number = rnd.nextInt(999999);
        else
            number = rnd.nextInt(99999);
        if(digit == 6)
            return String.format("%06d", number);
        return String.format("%05d", number);
    }

}
