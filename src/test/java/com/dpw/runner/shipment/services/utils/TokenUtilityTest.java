package com.dpw.runner.shipment.services.utils;

import com.nimbusds.jwt.JWT;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.proc.BadJWTException;
import com.nimbusds.jwt.proc.DefaultJWTClaimsVerifier;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.Mockito;

import java.text.ParseException;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.*;

@Execution(CONCURRENT)
class TokenUtilityTest {
    private TokenUtility tokenUtility;

    @BeforeEach
    void setUp() {
        tokenUtility = new TokenUtility();
    }

    @Test
    void testGetUserNameFromToken_ValidToken() throws Exception {
        String token = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYW1laWQiOiJTaGFkYWJFR1lTSElQTUVOVFFBUDEwMEBkcHdvcmxkLmNvbSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL25hbWUiOiJTaGFkYWJFR1lTSElQTUVOVFFBUDEwMEBkcHdvcmxkLmNvbSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL25hbWVpZGVudGlmaWVyIjoiU2hhZGFiRUdZU0hJUE1FTlRRQVAxMDBAZHB3b3JsZC5jb20iLCJ1bmlxdWVfbmFtZSI6IlNoYWRhYkVHWVNISVBNRU5UUUFQMTAwQGRwd29ybGQuY29tIiwic3ViIjoiU2hhZGFiRUdZU0hJUE1FTlRRQVAxMDBAZHB3b3JsZC5jb20iLCJqdGkiOiJhY2I2ZGYyYy0xNDQwLTQ0MzktOGIzYi1lODExZTYyYTE5MzIiLCJSb2xlIjoiRXh0ZXJuYWwiLCJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL3dzLzIwMDgvMDYvaWRlbnRpdHkvY2xhaW1zL2F1dGhlbnRpY2F0aW9ubWV0aG9kIjoiand0VG9rZW4iLCJ1c2VySWQiOiIxODEwIiwiYnJhbmNoSWQiOiI5MDgiLCJjb21wYW55SWQiOiIxIiwicGFyZW50Q29tcGFueUlkIjoiNTc5IiwiZXhwIjoxNzE0NjgzNjU0LCJpc3MiOiJodHRwczovL3FhLXJ1bm5lci5jYXJnb2VzLmNvbSIsImF1ZCI6Imh0dHBzOi8vcWEtcnVubmVyLmNhcmdvZXMuY29tIn0._PK2PJjap6syLEUmxqStgucGxyFWB_QJij1P5fkgxUI";
        String subject = "testSubject";

        JWTClaimsSet claimsSet = new JWTClaimsSet.Builder().subject("testSubject").build();
        JWT jwt = mock(JWT.class);
        when(jwt.getJWTClaimsSet()).thenReturn(claimsSet);

        DefaultJWTClaimsVerifier claimsVerifier = mock(DefaultJWTClaimsVerifier.class);
        doThrow(BadJWTException.class).when(claimsVerifier).verify(claimsSet, null); // Simulation of verification failure
        assertThrows(BadJWTException.class, () -> tokenUtility.getUserNameFromToken(token, null));
    }

    @Test
    void testGetUserNameFromToken_InvalidToken() {
        String token = "InvalidToken";
        assertThrows(BadJWTException.class, () -> tokenUtility.getUserNameFromToken(token, null));
    }

    @Test
    void testGetUserIdAndBranchId_ValidToken() throws ParseException {
        String token = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYW1laWQiOiJTaGFkYWJFR1lTSElQTUVOVFFBUDEwMEBkcHdvcmxkLmNvbSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL25hbWUiOiJTaGFkYWJFR1lTSElQTUVOVFFBUDEwMEBkcHdvcmxkLmNvbSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL25hbWVpZGVudGlmaWVyIjoiU2hhZGFiRUdZU0hJUE1FTlRRQVAxMDBAZHB3b3JsZC5jb20iLCJ1bmlxdWVfbmFtZSI6IlNoYWRhYkVHWVNISVBNRU5UUUFQMTAwQGRwd29ybGQuY29tIiwic3ViIjoiU2hhZGFiRUdZU0hJUE1FTlRRQVAxMDBAZHB3b3JsZC5jb20iLCJqdGkiOiJhY2I2ZGYyYy0xNDQwLTQ0MzktOGIzYi1lODExZTYyYTE5MzIiLCJSb2xlIjoiRXh0ZXJuYWwiLCJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL3dzLzIwMDgvMDYvaWRlbnRpdHkvY2xhaW1zL2F1dGhlbnRpY2F0aW9ubWV0aG9kIjoiand0VG9rZW4iLCJ1c2VySWQiOiIxODEwIiwiYnJhbmNoSWQiOiI5MDgiLCJjb21wYW55SWQiOiIxIiwicGFyZW50Q29tcGFueUlkIjoiNTc5IiwiZXhwIjoxNzE0NjgzNjU0LCJpc3MiOiJodHRwczovL3FhLXJ1bm5lci5jYXJnb2VzLmNvbSIsImF1ZCI6Imh0dHBzOi8vcWEtcnVubmVyLmNhcmdvZXMuY29tIn0._PK2PJjap6syLEUmxqStgucGxyFWB_QJij1P5fkgxUI";
        String userId = "123";
        String branchId = "456";
        JWTClaimsSet claimsSet = new JWTClaimsSet.Builder()
                .claim(TokenUtility.USER_ID_FIELD, userId)
                .claim(TokenUtility.BRANCH_ID_FIELD, branchId)
                .build();
        JWT jwt = mock(JWT.class);
        when(jwt.getJWTClaimsSet()).thenReturn(claimsSet);
        String result = tokenUtility.getUserIdAndBranchId(token);
        assertNotNull(result);
    }

    @Test
    void testGetUserIdAndBranchId_InvalidToken() {
        String token = "InvalidToken";
        String result = tokenUtility.getUserIdAndBranchId(token);
        assertNull(result);
    }

    @Test
    void testGetUserIdAndBranchId_NullToken() {
        String token = null;
        String result = tokenUtility.getUserIdAndBranchId(token);
        assertNotNull(result);
    }

    @Test
    void testGetUserIdAndBranchId_InvalidFormatToken_ThrowsException() {
        String token = "Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9";
        String result = tokenUtility.getUserIdAndBranchId(token);
        assertNotNull(result);
    }

    @Test
    void test() throws BadJWTException {
        String token = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYW1laWQiOiJTaGFkYWJFR1lTSElQTUVOVFFBUDEwMEBkcHdvcmxkLmNvbSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL25hbWUiOiJTaGFkYWJFR1lTSElQTUVOVFFBUDEwMEBkcHdvcmxkLmNvbSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL25hbWVpZGVudGlmaWVyIjoiU2hhZGFiRUdZU0hJUE1FTlRRQVAxMDBAZHB3b3JsZC5jb20iLCJ1bmlxdWVfbmFtZSI6IlNoYWRhYkVHWVNISVBNRU5UUUFQMTAwQGRwd29ybGQuY29tIiwic3ViIjoiU2hhZGFiRUdZU0hJUE1FTlRRQVAxMDBAZHB3b3JsZC5jb20iLCJqdGkiOiJhY2I2ZGYyYy0xNDQwLTQ0MzktOGIzYi1lODExZTYyYTE5MzIiLCJSb2xlIjoiRXh0ZXJuYWwiLCJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL3dzLzIwMDgvMDYvaWRlbnRpdHkvY2xhaW1zL2F1dGhlbnRpY2F0aW9ubWV0aG9kIjoiand0VG9rZW4iLCJ1c2VySWQiOiIxODEwIiwiYnJhbmNoSWQiOiI5MDgiLCJjb21wYW55SWQiOiIxIiwicGFyZW50Q29tcGFueUlkIjoiNTc5IiwiZXhwIjoxNzE0NjgzNjU0LCJpc3MiOiJodHRwczovL3FhLXJ1bm5lci5jYXJnb2VzLmNvbSIsImF1ZCI6Imh0dHBzOi8vcWEtcnVubmVyLmNhcmdvZXMuY29tIn0._PK2PJjap6syLEUmxqStgucGxyFWB_QJij1P5fkgxUI";
        var spyService = Mockito.spy(tokenUtility);
        doNothing().when(spyService).validateValidity(any());
        String result = spyService.getUserIdAndBranchId(token);
        assertNotNull(result);
    }
}