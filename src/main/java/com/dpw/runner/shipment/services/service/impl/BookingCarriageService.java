package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.dto.patchRequest.BookingCarriagePatchRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.BookingCarriageMapper;
import com.dpw.runner.shipment.services.service.interfaces.IBookingCarriageService;
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
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class BookingCarriageService implements IBookingCarriageService {
    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private BookingCarriageMapper bookingCarriageMapper;

    @Autowired
    private AuditLogService auditLogService;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        BookingCarriageRequest request = null;
        request = (BookingCarriageRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Booking Carriage Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        BookingCarriage bookingCarriage = bookingCarriageMapper.map(request);
        try {
            bookingCarriage = bookingCarriageDao.save(bookingCarriage);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(bookingCarriage)
                            .prevData(null)
                            .parent(BookingCarriage.class.getSimpleName())
                            .parentId(bookingCarriage.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Booking Carriage created successfully for Id {} with Request Id {}", bookingCarriage.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(bookingCarriage));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        BookingCarriageRequest request = (BookingCarriageRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for booking carriage update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.error("Request Id is null for booking carriage update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<BookingCarriage> oldEntity = bookingCarriageDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Booking Carriage is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        BookingCarriage bookingCarriage = bookingCarriageMapper.map(request);
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            bookingCarriage = bookingCarriageDao.save(bookingCarriage);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(bookingCarriage)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, BookingCarriage.class))
                            .parent(BookingCarriage.class.getSimpleName())
                            .parentId(bookingCarriage.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the booking carriage details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(bookingCarriage));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for booking carriage list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<BookingCarriage>, Pageable> tuple = fetchData(request, BookingCarriage.class);
            Page<BookingCarriage> bookingCarriagePage = bookingCarriageDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Booking carriage list retrieved successfully for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(bookingCarriagePage.getContent()),
                    bookingCarriagePage.getTotalPages(),
                    bookingCarriagePage.getTotalElements());
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
                log.error("Request is empty for booking carriage async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<BookingCarriage>, Pageable> tuple = fetchData(request, BookingCarriage.class);
            Page<BookingCarriage> bookingCarriagePage = bookingCarriageDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Booking carriage async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(bookingCarriagePage.getContent()),
                                    bookingCarriagePage.getTotalPages(),
                                    bookingCarriagePage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }

    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for booking carriage delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for booking carriage delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<BookingCarriage> bookingCarriage = bookingCarriageDao.findById(id);
            if (!bookingCarriage.isPresent()) {
                log.debug("Booking Carriage is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(bookingCarriage.get());
            bookingCarriageDao.delete(bookingCarriage.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, BookingCarriage.class))
                            .parent(BookingCarriage.class.getSimpleName())
                            .parentId(bookingCarriage.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted booking carriage for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for booking carriage retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for booking carriage retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<BookingCarriage> bookingCarriage = bookingCarriageDao.findById(id);
            if (!bookingCarriage.isPresent()) {
                log.debug("Booking Carriage is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Booking carriage fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            BookingCarriageResponse response = convertEntityToDto(bookingCarriage.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel) {
        String responseMsg;
        BookingCarriagePatchRequest request = (BookingCarriagePatchRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for booking carriage update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (request.getId() == null) {
            log.error("Request Id is null for booking carriage update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<BookingCarriage> oldEntity = bookingCarriageDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Booking Carriage is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        BookingCarriage existingEntity = bookingCarriageDao.findById(id).get();
        bookingCarriageMapper.update(request, existingEntity);
        try {
            existingEntity = bookingCarriageDao.save(existingEntity);
            log.info("Updated the booking carriage details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        // This can be achieved by model mapper also
        return ResponseHelper.buildSuccessResponse(bookingCarriageMapper.map(existingEntity));
    }

    private BookingCarriageResponse convertEntityToDto(BookingCarriage bookingCarriage) {
        return jsonHelper.convertValue(bookingCarriage, BookingCarriageResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<BookingCarriage> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(bookingCarriage -> {
            responseList.add(convertEntityToDto(bookingCarriage));
        });
        return responseList;
    }

    public BookingCarriage convertRequestToEntity(BookingCarriageRequest request) {
        return jsonHelper.convertValue(request, BookingCarriage.class);
    }
}
