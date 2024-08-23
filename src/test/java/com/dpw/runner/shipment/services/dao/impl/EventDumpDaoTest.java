package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.entity.EventsDump;
import com.dpw.runner.shipment.services.repository.interfaces.IEventDumpRepository;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EventDumpDaoTest {

    @Mock
    private IEventDumpRepository eventDumpRepository;

    @InjectMocks
    private EventDumpDao eventDumpDao;

    @Test
    void testSaveAll() {
        EventsDump eventsDump = new EventsDump();
        eventsDump.setId(1L);
        List<EventsDump> input = List.of(eventsDump);

        when(eventDumpRepository.saveAll(input)).thenReturn(input);

        var r = eventDumpDao.saveAll(input);

        assertNotNull(r);
        assertNotNull(r.get(0).getId());
    }

    @Test
    void testFindAll() {
        EventsDump eventsDump = new EventsDump();
        eventsDump.setId(1L);
        List<EventsDump> eventsList = List.of(eventsDump);

        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<EventsDump>, Pageable> pair = fetchData(listReq, EventsDump.class);

        Page<EventsDump> page = new PageImpl(eventsList);
        when(eventDumpRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(page);

        var r = eventDumpDao.findAll(pair.getLeft(), pair.getRight());

        assertNotNull(r.getContent());
    }

}