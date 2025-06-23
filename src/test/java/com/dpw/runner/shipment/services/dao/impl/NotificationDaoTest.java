package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INotificationRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NotificationDaoTest {
    @Mock
    private INotificationRepository notificationRepository;
    @Mock
    private ValidatorUtility validatorUtility;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private NotificationDao notificationDao;

    private static JsonTestUtility jsonTestUtility;
    private static Notification notification;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }


    @BeforeEach
    void setUp() {
        notification = jsonTestUtility.getNotification();
        UserContext.setUser(UsersDto.builder().Username("user").build());
    }

    @Test
    void testFindByEntityIdAndEntityType(){
        notificationDao.findByEntityIdAndEntityType(1L, Constants.SHIPMENT);
        verify(notificationRepository, times(1)).findByEntityIdAndEntityType(1L, Constants.SHIPMENT);
    }

    @Test
    void testDelete(){
        notificationDao.delete(any());
        verify(notificationRepository, times(1)).delete(any());
    }

    @Test
    void testFindByGuid(){
        UUID randomGuid = UUID.fromString("893cc8fa-7315-4d23-a635-3ce8705a5140");
        Mockito.when(notificationRepository.findByGuid(randomGuid)).thenReturn(Optional.of(notification));
        Optional<Notification> notification1 = notificationDao.findByGuid(randomGuid);
        verify(notificationRepository, times(1)).findByGuid(randomGuid);
        assertEquals(randomGuid, notification1.get().getGuid());
    }

    @Test
    void testFindById(){
        Long notificationId = 11L;
        Mockito.when(notificationRepository.findById(notificationId)).thenReturn(Optional.of(notification));
        Optional<Notification> notification1 = notificationDao.findById(notificationId);
        verify(notificationRepository, times(1)).findById(notificationId);
        assertEquals(notificationId, notification1.get().getId());
    }

    @Test
    void testFindAll(){
        Specification<Notification> spec = null;
        Pageable pageable = null;
        List<Notification> notificationList = new ArrayList<>();
        Page<Notification> notificationPage = new PageImpl<>(notificationList);
        Mockito.when(notificationRepository.findAll(spec, pageable)).thenReturn(notificationPage);
        Page<Notification> notifications = notificationDao.findAll(spec, pageable);
        assertEquals(notificationPage.getTotalElements(), notifications.getTotalElements());
    }

    @Test
    void testSaveSuccess() {
        Notification notification1 = new Notification();
        Mockito.when(notificationRepository.save(Mockito.any())).thenReturn(notification1);
        Notification notification2 = notificationDao.save(notification1);
        assertEquals(notification1, notification2);
    }

    @Test
    void testSaveWithEntitySuccess() {
        Notification newNotification = new Notification();
        newNotification.setId(21L);
        Mockito.when(notificationRepository.findById(21L)).thenReturn(Optional.of(newNotification));
        Mockito.when(notificationRepository.save(Mockito.any())).thenReturn(newNotification);
        Notification notification1 = notificationDao.save(newNotification);
        assertEquals(newNotification, notification1);
        verify(notificationRepository, times(1)).findById(21L);
        verify(notificationRepository, times(1)).save(any());
    }

    @Test
    void testSaveWithEntityFailure() {
        HashSet<String> error = new HashSet<>();

        Notification notification1 = Notification.builder().build();
        notification1.setId(1L);

        when(notificationRepository.findById(any())).thenReturn(Optional.empty());
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(DataRetrievalFailureException.class, () -> {
            notificationDao.save(notification1);
        });
    }

    @Test
    void SaveError() {
        HashSet<String> error = new HashSet<>();
        error.add("An Error Occurred");
        Notification notification1 = Notification.builder().build();
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(ValidationException.class, () -> {
            notificationDao.save(notification1);
        });
    }

    @Test
    void testSaveAllSuccess() {
        Notification notification1 = new Notification();
        Notification notification2 = new Notification();
        List<Notification> notifications = List.of(notification1, notification2);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(validatorUtility.applyValidation(any(), any(), any(), eq(false))).thenReturn(new HashSet<>());
        when(notificationRepository.saveAll(notifications)).thenReturn(notifications);

        List<Notification> savedNotifications = notificationDao.saveAll(notifications);

        assertEquals(notifications, savedNotifications);
        verify(jsonHelper, times(2)).convertToJson(any());
        verify(validatorUtility, times(2)).applyValidation(any(), eq(Constants.NOTIFICATION_ENTITY), eq(LifecycleHooks.ON_CREATE), eq(false));
        verify(notificationRepository, times(1)).saveAll(notifications);
    }

    @Test
    void testSaveAllValidationFailure() {
        Notification notification1 = new Notification();
        List<Notification> notifications = List.of(notification1);

        Set<String> errors = Set.of("Validation error");
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(validatorUtility.applyValidation(any(), any(), any(), eq(false))).thenReturn(errors);

        ValidationException exception = assertThrows(ValidationException.class, () -> notificationDao.saveAll(notifications));
        assertEquals("Validation error", exception.getMessage());

        verify(jsonHelper, times(1)).convertToJson(any());
        verify(validatorUtility, times(1)).applyValidation(any(), eq(Constants.NOTIFICATION_ENTITY), eq(LifecycleHooks.ON_CREATE), eq(false));
        verify(notificationRepository, never()).saveAll(any());
    }

    @Test
    void testDeleteAll() {
        notificationDao.deleteAll(anyList());
        verify(notificationRepository, times(1)).deleteAll(anyList());
    }

    @Test
    void testFindNotificationForEntityTransfer() {
        notificationDao.findNotificationForEntityTransfer(anyLong(), anyString(), anyInt(), anyList());
        verify(notificationRepository, times(1)).
                findNotificationBasedOnEntityIdAndEntityTypeAndBranchIdAndRequestTypes(anyLong(), anyString(), anyInt(), anyList());
    }

    @Test
    void testPendingNotificationCountBasedOnEntityIdsAndEntityType() {
        List<Long> entityIds = List.of(1L, 2L, 3L);
        String entityType = "SampleEntityType";

        List<Object[]> mockResults = new ArrayList<>();
        mockResults.add(new Object[]{1L, 5});
        mockResults.add(new Object[]{2L, 10});
        mockResults.add(new Object[]{3L, 15});

        when(notificationRepository.pendingNotificationCountBasedOnEntityIdsAndEntityType(entityIds, entityType))
                .thenReturn(mockResults);

        Map<Long, Integer> result = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(entityIds, entityType);

        Map<Long, Integer> expected = Map.of(
                1L, 5,
                2L, 10,
                3L, 15
        );
        assertEquals(expected, result);

        verify(notificationRepository, times(1)).pendingNotificationCountBasedOnEntityIdsAndEntityType(entityIds, entityType);
    }

    @Test
    void testPendingNotificationCountWithEmptyResults() {
        List<Long> entityIds = List.of(1L, 2L, 3L);
        String entityType = "SampleEntityType";

        when(notificationRepository.pendingNotificationCountBasedOnEntityIdsAndEntityType(entityIds, entityType))
                .thenReturn(Collections.emptyList());

        Map<Long, Integer> result = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(entityIds, entityType);

        assertEquals(Collections.emptyMap(), result);

        verify(notificationRepository, times(1)).pendingNotificationCountBasedOnEntityIdsAndEntityType(entityIds, entityType);
    }

    @Test
    void testFindNotificationByEntityIdsForEntityTransfer() {
        notificationDao.findNotificationByEntityIdsForEntityTransfer(anyList(), anyString(), anyInt(), anyList());
        verify(notificationRepository, times(1)).
                findNotificationBasedOnEntityIdsAndEntityTypeAndBranchIdAndRequestTypes(anyList(), anyString(), anyInt(), anyList());
    }

}