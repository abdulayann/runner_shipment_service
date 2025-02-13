package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.nimbusds.jwt.JWT;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.JWTParser;
import com.nimbusds.jwt.proc.BadJWTException;
import com.nimbusds.jwt.proc.DefaultJWTClaimsVerifier;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletResponse;
import java.text.ParseException;
import java.util.Objects;

@Component
@Slf4j
public class TokenUtility {
    public static final String NAME_FIELD = "nameid";
    public static final String USER_ID_FIELD = "userId";
    public static final String BRANCH_ID_FIELD = "branchId";
    private static final String BEARER = "Bearer";

    public String getUserNameFromToken(String token, HttpServletResponse res) throws ParseException, BadJWTException {
        String[] tokenSplits = token.split(" ");
        if (tokenSplits.length > 2 || !BEARER.equals(tokenSplits[0]))
            throw new BadJWTException("Expected 'Bearer token'");
        JWT parse = JWTParser.parse(tokenSplits[tokenSplits.length - 1]);

        JWTClaimsSet claimsSet = parse.getJWTClaimsSet();
        validateValidity(claimsSet);
        return claimsSet.getSubject();
    }

    public void validateValidity(JWTClaimsSet claimsSet) throws BadJWTException {
        DefaultJWTClaimsVerifier defaultJWTClaimsVerifier = new DefaultJWTClaimsVerifier(claimsSet, null);
        defaultJWTClaimsVerifier.verify(claimsSet, null);
    }

    public String getUserIdAndBranchId(String token) {
        try {
            if (token.split(" ").length <= 1 || !Objects.equals(token.split(" ")[0], BEARER))
                return null;
            token = token.split(" ")[1];

            JWT parse = JWTParser.parse(token);
            JWTClaimsSet claimsSet = parse.getJWTClaimsSet();
            validateValidity(claimsSet);
            String key = claimsSet.getClaim(USER_ID_FIELD) + "|" + claimsSet.getClaim(BRANCH_ID_FIELD);
            log.info("Token key for RequestId {} is {}", LoggerHelper.getRequestIdFromMDC(), key);
            return key;
        } catch (Exception ex) {
            log.error("Request- {} || Error occurred during token decryption with exception: {} token: {}", LoggerHelper.getRequestIdFromMDC(), ex.getMessage(), token);
        }
        return StringUtility.convertToString(Math.random());
    }

}
